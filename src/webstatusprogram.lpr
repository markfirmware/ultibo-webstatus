program WebStatusProgram;
{$mode delphi}{$h+}

uses
 {$ifdef CONTROLLER_RPI_INCLUDING_RPI0}  BCM2835,BCM2708,PlatformRPi,                 {$endif}
 {$ifdef CONTROLLER_RPI2_INCLUDING_RPI3} BCM2836,BCM2709,PlatformRPi2,                {$endif}
 {$ifdef CONTROLLER_RPI3}                BCM2837,BCM2710,PlatformRPi3,                {$endif}
 {$ifdef CONTROLLER_QEMUVPB}             QEMUVersatilePB,PlatformQemuVpb,VersatilePB, {$endif}

 Classes,Console,GlobalConfig,GlobalConst,GlobalTypes,
 HeapManager,HTTP,Ip,Logging,Mouse,Network,Platform,Rtc,Serial,
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
 X:=ConsoleWhereX;
 Y:=ConsoleWhereY;
 ConsoleGotoXY(1,5);
 ConsoleClrEol;
 ConsoleGotoXY(X,Y);
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

const
 HEAP_FLAG_SYSTEM_RESET_HISTORY=$08000000;

type
 PPersistentStorage = ^TPersistentStorage;
 TPersistentStorage = packed record
  ReservedForHeapManager:array[0..31] of Byte;
  Signature:LongWord;
  ResetCount:LongWord;
  ClockCountAtLastReset:LongWord;
  ClockCountForColdStart:LongWord;
  StartingClockCount:LongWord;
 end;

 TSystemRestartHistory = record
  Valid:Boolean;
  Storage:PPersistentStorage;
  constructor Create(Where:Pointer);
  function GetResetCount:LongWord;
  function SecondsSinceLastReset:Double;
  procedure SetClockCountAtLastReset(Count:LongWord);
 end;

var
 IpAddress:String;
 EffectiveIpAddress:String;
 HTTPListener:THTTPListener;

constructor TSystemRestartHistory.Create(Where:Pointer);
const
 InitializationSignature=$73AF72EC;
begin
 Storage:=PPersistentStorage(RequestHeapBlock(Where,1*1024*1024,HEAP_FLAG_SYSTEM_RESET_HISTORY,CPU_AFFINITY_NONE));
 Valid:=Storage = Where;
 if Valid then
  begin
   Storage.StartingClockCount:=ClockGetCount;
   if Storage.Signature <> InitializationSignature then
    begin
     Storage.Signature:=InitializationSignature;
     Storage.ResetCount:=0;
     Storage.ClockCountAtLastReset:=0;
     Storage.ClockCountForColdStart:=Storage.StartingClockCount;
    end
   else
    begin
     Inc(Storage.ResetCount);
    end;
  end;
end;

function TSystemRestartHistory.GetResetCount:LongWord;
begin
 if Valid then
  Result:=Storage.ResetCount
 else
  Result:=0;
end;

function TSystemRestartHistory.SecondsSinceLastReset:Double;
begin
 if Valid then
  begin
   Result:=(ClockGetCount - Storage.ClockCountAtLastReset) / (1000*1000);
  end
 else
  begin
   Result:=0;
  end;
end;

procedure TSystemRestartHistory.SetClockCountAtLastReset(Count:LongWord);
begin
 if Valid then
  Storage.ClockCountAtLastReset:=Count;
end;

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
 Log(Format('Command Line = <%s>',[GetCommandLine]));
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

var
 Window:TWindowHandle;

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

var
 SystemRestartHistory:TSystemRestartHistory;

procedure SystemRestartWithHistory;
begin
 {$ifdef CONTROLLER_QEMUVPB}
  ConsoleWindowSetBackColor(Window,COLOR_YELLOW);
  ConsoleClrScr;
  Log('');
  Log('system reset initiated');
  Sleep(1 * 1000);
  Log('this can take up to 5 seconds ...');
  SystemRestartHistory.SetClockCountAtLastReset(ClockGetCount);
  SystemRestart(0);
 {$endif} 
end;

procedure Main;
var
 MouseData:TMouseData;
 MouseCount:Cardinal;
 MouseButtons:Word;
 MouseOffsetX,MouseOffsetY,MouseOffsetWheel:Integer;
 I,X,Y:Cardinal;
 Key:Char;
 CapturedClockGetTotal,CapturedSysRtcGetTime:Int64;
 AdjustedRtc,RtcAdjustment:Int64;
begin
 SystemRestartHistory:=TSystemRestartHistory.Create(Pointer((64 - 1) * 1024*1024));
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
 ConsoleWindowSetBackColor(Window,COLOR_WHITE);
 ConsoleClrScr;
 RtcAdjustment:=SysRtcGetTime - ClockGetTotal * 10;
 BuildNumber:=0;
 FrameMeter:=TRateMeter.Create;
 MouseMeter:=TRateMeter.Create;
 MouseOffsetX:=0;
 MouseOffsetY:=0;
 MouseOffsetWheel:=0;
 MouseButtons:=0;
 QemuHostIpAddress:='';
 DetermineEntryState;
 StartLogging;
 Sleep(500);
 for I:=1 to 5 do
  Log ('');
 Log('program start');
 Log(Format('Reset count %d Time since last reset %5.3f seconds',[SystemRestartHistory.GetResetCount,SystemRestartHistory.SecondsSinceLastReset]));
 ParseCommandLine;
 Log(Format('Ultibo Release %s %s %s',[ULTIBO_RELEASE_DATE,ULTIBO_RELEASE_NAME,ULTIBO_RELEASE_VERSION]));
 if Controller = QemuVpb then
  begin
   EffectiveIpAddress:=QemuHostIpAddress;
   Log(Format('Web Server Effective URL (running under QEMU) is http://%s:557%s',[EffectiveIpAddress,QemuHostIpPortDigit]));
  end
 else
  begin
   IpAddress:=GetIpAddress;
   EffectiveIpAddress:=IpAddress;
   Log(Format('IP address %s',[EffectiveIpAddress]));
  end;
 StartHttpServer;
 Log('');
 Log('r key will reset system');
 Log('Ready and waiting for input ...');
 Log ('');
 if InService then
  while True do
   begin
    CapturedSysRtcGetTime:=SysRtcGetTime;
    CapturedClockGetTotal:=ClockGetTotal;
    AdjustedRtc:=(CapturedSysRtcGetTime - RtcAdjustment) div (10*1000*1000) * (10*1000*1000);
    FrameMeter.Increment;
    if ConsoleKeyPressed then
     begin
      Key:=ConsoleReadKey;
      Log(Format('KeyPressed <%s> %d',[Key,Ord(Key)]));
      if Key = 'r' then
       begin
        SystemRestartWithHistory;
       end;
      if Ord(Key) = 163 then
       begin
        Log('power reset initiated');
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
        MouseOffsetWheel:=MouseData.OffsetWheel;
        MouseButtons:=MouseData.Buttons;
       end
      else
       begin
        MouseMeter.FlushInSeconds(0.3);
        if MouseMeter.GetCount = 0 then
         begin
          MouseOffsetX:=0;
          MouseOffsetY:=0;
          MouseOffsetWheel:=0;
         end;
        break;
       end;
     end;
    X:=ConsoleWhereX;
    Y:=ConsoleWhereY;
    ConsoleGotoXY(20,1);
    Write(Format('   Frame Count %3d Rate %5.1f Hz',[FrameMeter.GetCount,FrameMeter.RateInHz]));
    ConsoleClrEol;
    ConsoleGotoXY(20,2);
    Write(Format('   Mouse Count %3d Rate %5.1f Hz dx %4d dy %4d dw %2d Buttons %4.4x',[MouseMeter.Count,MouseMeter.RateInHz,MouseOffsetX,MouseOffsetY,MouseOffsetWheel,MouseButtons]));
    ConsoleClrEol;
    ConsoleGotoXY(20,3);
    Write(Format('   Clock %8d RTC (adjusted) %9d',[CapturedClockGetTotal,AdjustedRtc]));
    ConsoleClrEol;
    ConsoleGotoXY(20,4);
    ConsoleClrEol;
    ConsoleGotoXY(X,Y);
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
