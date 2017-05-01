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
 HTTPListener:THTTPListener;

function GetIpAddress:String;
var
 Winsock2TCPClient:TWinsock2TCPClient;
begin
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

type
 TWebAboutStatus = class (TWebStatusCustom)
  function DoContent(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 end;

function TWebAboutStatus.DoContent(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
begin
 Log('TWebAboutStatus.DoContent');
 AddItem(AResponse,'QEMU vnc server','host 45.79.200.166 port 5970');
 AddItem(AResponse,'Web Browser vncviewer',MakeLink('use host 45.79.200.166 port 5770 - note port 5770, not 5970','http://novnc.com/noVNC/vnc.html'));
 AddItem(AResponse,'CircleCI Build:',MakeLink('Build #30 markfirmware/ultibo-webstatus (branch test-20170425)','https://circleci.com/gh/markfirmware/ultibo-webstatus/30#artifacts/containers/0'));
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

procedure LogCommandLine;
var
 I:Cardinal;
begin
 for I:=0 to ParamCount do
  Log(Format('Param %d = %s',[I,ParamStr(I)]));
end;

procedure SystemReset;
begin
 {$ifdef CONTROLLER_QEMUVPB}
  WriteLn('System Reset Requested');
  Sleep(1*1000);
  PLongWord(VERSATILEPB_SYS_LOCK)^:=$a05f;
  PLongWord(VERSATILEPB_SYS_RESETCTL)^:=$4;
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
 DetermineEntryState;
 StartLogging;
 Sleep(1000);
 Log('');
 LogCommandLine;
 IpAddress:=GetIpAddress;
 StartHttpServer;
 Log(Format('BoardType %s',[BoardTypeToString(BoardGetType)]));
 Log(Format('Ultibo Release %s %s %s',[ULTIBO_RELEASE_DATE,ULTIBO_RELEASE_NAME,ULTIBO_RELEASE_VERSION]));
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