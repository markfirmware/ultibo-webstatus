program WebStatusProgram;
{$mode delphi}{$h+}

uses
 {$ifdef CONTROLLER_RPI_INCLUDING_RPI0}  BCM2835,BCM2708,PlatformRPi,                 {$endif}
 {$ifdef CONTROLLER_RPI2_INCLUDING_RPI3} BCM2836,BCM2709,PlatformRPi2,                {$endif}
 {$ifdef CONTROLLER_RPI3}                BCM2837,BCM2710,PlatformRPi3,                {$endif}
 {$ifdef CONTROLLER_QEMUVPB}             QEMUVersatilePB,PlatformQemuVpb,VersatilePB, {$endif}

 Classes,Crt,GlobalConfig,GlobalConst,
 HTTP,Ip,Logging,Mouse,Network,Platform,Serial,
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
begin
 LoggingOutput(S);
 WriteLn(S);
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
     Log(Format('Build Number %d',[BuildNumber]));
    end;
   if AnsiStartsStr('qemuhostip=',Param) then
    begin
     Start:=PosEx('=',Param);
     QemuHostIpAddress:=MidStr(Param,Start + 1,Length(Param) - Start);
     Log(Format('QEMU Host IP %s',[QemuHostIpAddress]));
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
 AddItem(AResponse,'QEMU vnc server',Format('host %s port 5970',[EffectiveIpAddress]));
 AddItem(AResponse,'Web Browser vncviewer',MakeLink(Format('use host %s port 5770 - note port 5770, not 5970',[EffectiveIpAddress]),'http://novnc.com/noVNC/vnc.html'));
 AddItem(AResponse,'CircleCI Build:',MakeLink(Format('Build #%d markfirmware/ultibo-webstatus (branch test-20170425)',[BuildNumber]),Format('https://circleci.com/gh/markfirmware/ultibo-webstatus/%d#artifacts/containers/0',[BuildNumber])));
 AddItem(AResponse,'GitHub Source:',MakeLink('markfirmware/ultibo-webstatus (branch test-20170425)','https://github.com/markfirmware/ultibo-webstatus/tree/test-20170425'));
 Result:=True;
end;

var
 AboutStatus:TWebAboutStatus;

procedure StartHttpServer;
begin
 HTTPListener:=THTTPListener.Create;
 WebStatusRegister(HTTPListener,'','',True);
 AboutStatus:=TWebAboutStatus.Create('About','/about',2);
 HTTPListener.RegisterDocument('',AboutStatus);
 HTTPListener.Active:=True;
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

procedure Main;
var
 MouseData:TMouseData;
 MouseCount:Cardinal;
 X,Y:Cardinal;
 Key:Char;
begin
 BuildNumber:=0;
 QemuHostIpAddress:='';
 DetermineEntryState;
 StartLogging;
 Log('program start');
 Sleep(1000);
 Log('');
 ParseCommandLine;
 Log(Format('BoardType %s',[BoardTypeToString(BoardGetType)]));
 Log(Format('Ultibo Release %s %s %s',[ULTIBO_RELEASE_DATE,ULTIBO_RELEASE_NAME,ULTIBO_RELEASE_VERSION]));
 if Controller = QemuVpb then
  begin
   EffectiveIpAddress:=QemuHostIpAddress;
   Log('');
   Log(Format('Effective IP Address (running under QEMU) is %s',[EffectiveIpAddress]));
  end
 else
  begin
   IpAddress:=GetIpAddress;
   EffectiveIpAddress:=IpAddress;
  end;
 StartHttpServer;
 Log('');
 Log('r key will reset system');
 Log('Note that if connected through vnc, the mouse must be moved to push the key through');
 Log('Ready and waiting for input ...');
 Log ('');
 if InService then
  while True do
   begin
    if KeyPressed then
     begin
      Key:=ReadKey;
      WriteLn(Format('KeyPressed %s',[Key]));
      if Key = 'r' then
       begin
        SystemReset;
       end;
     end;
    if MouseRead(@MouseData,SizeOf(TMouseData),MouseCount) = ERROR_SUCCESS then
     begin
      X:=WhereX;
      Y:=WhereY;
      GotoXY(80,1);
      Write(Format('%d %d      ',[MouseData.OffsetX,MouseData.OffsetY]));
      GotoXY(X,Y);
     end;
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
