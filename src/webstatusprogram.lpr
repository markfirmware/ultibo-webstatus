program WebStatusProgram;
{$mode delphi}{$h+}

uses
 {$ifdef CONTROLLER_RPI_INCLUDING_RPI0}  BCM2835,BCM2708,PlatformRPi,                 {$endif}
 {$ifdef CONTROLLER_RPI2_INCLUDING_RPI3} BCM2836,BCM2709,PlatformRPi2,                {$endif}
 {$ifdef CONTROLLER_RPI3}                BCM2837,BCM2710,PlatformRPi3,                {$endif}
 {$ifdef CONTROLLER_QEMUVPB}             QEMUVersatilePB,PlatformQemuVpb,VersatilePB, {$endif}

 Classes,Crt,GlobalConfig,GlobalConst,
 HTTP,Ip,Logging,Mouse,Network,Platform,Rtc,Serial,
 StrUtils,SysUtils,Ultibo,WebStatus,Winsock2;

type
 TController = (Rpi, Rpi2, Rpi3, QemuVpb);

function ControllerToString(Controller:TController):String;
begin
 case Controller of
  Rpi: ControllerToString:='Rpi';
  Rpi2: ControllerToString:='Rpi2';
  Rpi3: ControllerToString:='Rpi3';
  QemuVpb: ControllerToString:='QemuVpb';
 end;
end;

var
 Controller:TController;

procedure DetermineEntryState;
begin
 Controller:={$ifdef CONTROLLER_RPI_INCLUDING_RPI0}  Rpi     {$endif}
             {$ifdef CONTROLLER_RPI2_INCLUDING_RPI3} Rpi2    {$endif}
             {$ifdef CONTROLLER_RPI3}                Rpi3    {$endif}
             {$ifdef CONTROLLER_QEMUVPB}             QemuVpb {$endif};
end;

procedure Log(S:String);
var
 X,Y:Cardinal;
begin
 LoggingOutput(S);
 WriteLn(S);
 X:=WhereX;
 Y:=WhereY;
 GotoXY(1,6);
 ClrEol;
 GotoXY(X,Y);
end;

procedure StartLogging;
begin
 if (Controller = QemuVpb) then
  begin
   SERIAL_REGISTER_LOGGING:=True;
   SerialLoggingDeviceAdd(SerialDeviceGetDefault);
   SERIAL_REGISTER_LOGGING:=False;
   LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_SERIAL));
  end
 else
  begin
   LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultiboslideshow.log');
   LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));
  end;
end;

function InService:Boolean;
begin
 Result := not ((ParamCount >= 1) and (ParamStr(1)='postbuildtest'));
end;

procedure Check(Status:Integer);
begin
 if Status <> 0 then
  raise Exception.Create(Format('Exception - status code %d',[Status]));
end;

procedure CheckNil(P:Pointer);
begin
 if not Assigned(P) then
  raise Exception.Create('Exception - unassigned pointer');
end;

var
 IpAddress:String;
 EffectiveIpAddress:String;
 HTTPListener:THTTPListener;

function GetIpAddress:String;
var
 Winsock2TCPClient:TWinsock2TCPClient;
begin
 Log('');
 Log('Obtaining IP address ...');
 Winsock2TCPClient:=TWinsock2TCPClient.Create;
 Result:=Winsock2TCPClient.LocalAddress;
 while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do
  begin
   Sleep(100);
   Result:=Winsock2TCPClient.LocalAddress;
  end;
 Winsock2TCPClient.Free;
 Log(Format('IP address %s',[Result]));
end;

var
 BuildNumber:Cardinal;
 QemuHostIpAddress:String;
 QemuHostIpPortDigit:String;

procedure ParseCommandLine;
var
 I,Start:Cardinal;
 Param:String;
begin
 for I:=0 to ParamCount do
  begin
   Param:=ParamStr(I);
   Log(Format('Param %d = %s',[I,Param]));
   if AnsiStartsStr('buildnumber=',Param) then
    begin
     Start:=PosEx('=',Param);
     BuildNumber:=StrToInt(MidStr(Param,Start + 1,Length(Param) - Start));
    end;
   if AnsiStartsStr('qemuhostip=',Param) then
    begin
     Start:=PosEx('=',Param);
     QemuHostIpAddress:=MidStr(Param,Start + 1,Length(Param) - Start);
    end;
   if AnsiStartsStr('qemuhostportdigit=',Param) then
    begin
     Start:=PosEx('=',Param);
     QemuHostIpPortDigit:=MidStr(Param,Start + 1,Length(Param) - Start);
    end;
  end;
end;

type
 TWebAboutStatus = class (TWebStatusCustom)
  function DoContent(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 end;

function TWebAboutStatus.DoContent(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
begin
 Log('TWebAboutStatus.DoContent');
 AddItem(AResponse,'QEMU vnc server',Format('Connect vnc viewer to host %s port 597%s or ',[EffectiveIpAddress,QemuHostIpPortDigit]) + MakeLink('use web browser',Format('http://novnc.com/noVNC/vnc_auto.html?host=%s&port=577%s&reconnect=1&reconnect_delay=5000',[EffectiveIpAddress,QemuHostIpPortDigit])));
 AddItem(AResponse,'CircleCI Build:',MakeLink(Format('Build #%d markfirmware/ultibo-webstatus (branch test-20170425)',[BuildNumber]),Format('https://circleci.com/gh/markfirmware/ultibo-webstatus/%d#artifacts/containers/0',[BuildNumber])));
 AddItem(AResponse,'GitHub Source:',MakeLink('markfirmware/ultibo-webstatus (branch test-20170425)','https://github.com/markfirmware/ultibo-webstatus/tree/test-20170425'));
 Result:=True;
end;

var
 AboutStatus:TWebAboutStatus;

procedure StartHttpServer;
var
 HTTPClient:THTTPClient;
 ResponseBody:String;
begin
 HTTPListener:=THTTPListener.Create;
 WebStatusRegister(HTTPListener,'','',True);
 AboutStatus:=TWebAboutStatus.Create('About','/about',2);
 HTTPListener.RegisterDocument('',AboutStatus);
 HTTPListener.Active:=True;
 HTTPClient:=THTTPClient.Create;
 if not HTTPClient.GetString('http://127.0.0.1/status/about',ResponseBody) then
  Log(Format('HTTPClient error %d %s',[HTTPClient.ResponseStatus,HTTPClient.ResponseReason]))
end;

procedure SystemReset;
{$ifdef CONTROLLER_QEMUVPB}
var
 SysResetRegister:LongWord;
{$endif} 
begin
 {$ifdef CONTROLLER_QEMUVPB}
  Log('');
  Log('system reset requested');
  Sleep(1 * 1000);
  PLongWord(VERSATILEPB_SYS_LOCK)^:=$a05f;
  SysResetRegister:=PLongWord(VERSATILEPB_SYS_LOCK)^;
  SysResetRegister:=SysResetRegister or $105;
  PLongWord(VERSATILEPB_SYS_RESETCTL)^:=SysResetRegister;
  PLongWord(VERSATILEPB_SYS_LOCK)^:=$0;
 {$endif} 
end;

type
 TRateMeter = class
  Count:Cardinal;
  FirstClock:Int64;
  LastClock:Int64;
  constructor Create;
  procedure Increment;
  procedure FlushInSeconds(Time:Double);
  procedure Reset;
  function RateInHz:Double;
  function GetCount:Cardinal;
 end;

var
 FrameMeter:TRateMeter;
 MouseMeter:TRateMeter;

constructor TRateMeter.Create;
begin
 Reset;
end;

procedure TRateMeter.Reset;
begin
 Count:=0;
 FirstClock:=ClockGetTotal;
 LastClock:=FirstClock;
end;

procedure TRateMeter.Increment;
begin
 Inc(Count);
 LastClock:=ClockGetTotal;
end;

function TRateMeter.RateInHz:Double;
var
 Delta:Double;
begin
 Delta:=LastClock;
 Delta:=Delta - FirstClock;
 if Delta >= 500 * 1000 then
  begin
   Result:=1000.0 * 1000.0 * Count / Delta;
  end
 else
  Result:=0;
end;

procedure TRateMeter.FlushInSeconds(Time:Double);
begin
 if ClockGetTotal - LastClock >= 1000 * 1000 * Time then
  Reset;
end;

function TRateMeter.GetCount:Cardinal;
begin
 Result:=Count;
end;

procedure Main;
var
 MouseData:TMouseData;
 MouseCount:Cardinal;
 MouseOffsetX,MouseOffsetY:Integer;
 I,X,Y:Cardinal;
 Key:Char;
 Clock,InitialClock,ClockSecondsValue,Rtc,InitialRtc,TimeDelta:Int64;
begin
 InitialRtc:=SysRtcGetTime;
 InitialClock:=ClockGetTotal;
 BuildNumber:=0;
 FrameMeter:=TRateMeter.Create;
 MouseMeter:=TRateMeter.Create;
 MouseOffsetX:=0;
 MouseOffsetY:=0;
 QemuHostIpAddress:='';
 DetermineEntryState;
 StartLogging;
 Sleep(500);
 for I:=1 to 6 do
  Log ('');
 Log('program start');
 ParseCommandLine;
 Log(Format('Ultibo Release %s %s %s',[ULTIBO_RELEASE_DATE,ULTIBO_RELEASE_NAME,ULTIBO_RELEASE_VERSION]));
 if Controller = QemuVpb then
  begin
   EffectiveIpAddress:=QemuHostIpAddress;
   Log('');
   Log(Format('Web Server Effective URL (running under QEMU) is http://%s:8%s',[EffectiveIpAddress,QemuHostIpPortDigit]));
  end
 else
  begin
   IpAddress:=GetIpAddress;
   EffectiveIpAddress:=IpAddress;
  end;
 StartHttpServer;
 Log('');
 Log('r key will reset system');
 Log('Ready and waiting for input ...');
 Log ('');
 if InService then
  while True do
   begin
    Rtc:=SysRtcGetTime;
    Clock:=ClockGetTotal;
    ClockSecondsValue:=ClockSeconds - InitialClock div (1000*1000);
    Rtc:=Rtc - InitialRtc;
    Rtc:=Rtc div 10;
    Clock:=Clock - InitialClock;
    TimeDelta:=Clock;
    TimeDelta:=TimeDelta - Rtc;
    FrameMeter.Increment;
    if KeyPressed then
     begin
      Key:=ReadKey;
      Log(Format('KeyPressed <%s> %d',[Key,Ord(Key)]));
      if Key = 'r' then
       begin
        SystemReset;
       end;
      if Ord(Key) = 163 then
       begin
        Log('power reset requested');
       end;
     end;
    while True do
     begin
      MouseCount:=0;
      MouseReadEx(@MouseData,SizeOf(TMouseData),MOUSE_FLAG_NON_BLOCK,MouseCount);
      if MouseCount > 0 then
       begin
        MouseMeter.Increment;
        MouseOffsetX:=MouseData.OffsetX;
        MouseOffsetY:=MouseData.OffsetY;
       end
      else
       begin
        MouseMeter.FlushInSeconds(0.3);
        break;
       end;
     end;
    X:=WhereX;
    Y:=WhereY;
    GotoXY(20,1);
    Write(Format('   Frame Count %3d Rate %5.1f Hz',[FrameMeter.GetCount,FrameMeter.RateInHz]));
    ClrEol;
    GotoXY(20,2);
    Write(Format('   Mouse Count %3d Rate %5.1f Hz dx %d dy %d',[MouseMeter.Count,MouseMeter.RateInHz,MouseOffsetX,MouseOffsetY]));
    ClrEol;
    GotoXY(20,3);
    Write(Format('   RTC %d Clock %d Delta %8d',[Rtc,Clock,TimeDelta]));
    ClrEol;
    GotoXY(20,4);
    Write(Format('   ClockSeconds %d Error %d',[ClockSecondsValue, Clock div (1000*1000) - ClockSecondsValue]));
    ClrEol;
    GotoXY(X,Y);
   end;
end;

begin
 try
  Main;
 except on E:Exception do
  begin
   Log(Format('Exception.Message %s',[E.Message]));
   if InService then
    Sleep(5000);
  end;
 end;
 Log('program stop');
end.
