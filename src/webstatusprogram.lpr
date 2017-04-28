program WebStatusProgram;
{$mode delphi}{$h+}

uses
 {$ifdef CONTROLLER_RPI_INCLUDING_RPI0}  BCM2835,BCM2708,PlatformRPi      {$endif}
 {$ifdef CONTROLLER_RPI2_INCLUDING_RPI3} BCM2836,BCM2709,PlatformRPi2     {$endif}
 {$ifdef CONTROLLER_RPI3}                BCM2837,BCM2710,PlatformRPi3     {$endif}
 {$ifdef CONTROLLER_QEMUVPB}             QEMUVersatilePB,PlatformQemuVpb, {$endif}

 Classes,Crt,GlobalConfig,GlobalConst,
 HTTP,Ip,Logging,Network,Platform,Serial,
 StrUtils,SysUtils,Ultibo,WebStatus,Winsock2;

type
 TTarget = (Rpi, Rpi2, Rpi3, QemuVpb);

function TargetToString(Target:TTarget):String;
begin
 case Target of
  Rpi: TargetToString:='Rpi';
  Rpi2: TargetToString:='Rpi2';
  Rpi3: TargetToString:='Rpi3';
  QemuVpb: TargetToString:='QemuVpb';
 end;
end;

var
 Target:TTarget;

procedure DetermineEntryState;
begin
 Target:={$ifdef CONTROLLER_RPI_INCLUDING_RPI0}  Rpi     {$endif}
         {$ifdef CONTROLLER_RPI2_INCLUDING_RPI3} Rpi2    {$endif}
         {$ifdef CONTROLLER_RPI3}                Rpi3    {$endif}
         {$ifdef CONTROLLER_QEMUVPB}             QemuVpb {$endif};
end;

procedure StartLogging;
begin
 if (Target = QemuVpb) then
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
 LoggingOutput(Format('IP address %s',[Result]));
end;

procedure StartHttpServer;
begin
 HTTPListener:=THTTPListener.Create;
 WebStatusRegister(HTTPListener,'','',True);
 HTTPListener.Active:=True;
end;

procedure LogCommandLine;
var
 I:Cardinal;
begin
 for I:=0 to ParamCount do
  LoggingOutput(Format('Param %d = %s',[I,ParamStr(I)]));
end;

procedure Main;
begin
 DetermineEntryState;
 StartLogging;
 Sleep(1000);
 LoggingOutput('');
 LogCommandLine;
 IpAddress:=GetIpAddress;
 StartHttpServer;
 LoggingOutput(Format('BoardType %s',[BoardTypeToString(BoardGetType)]));
 LoggingOutput(Format('Ultibo Release %s %s %s',[ULTIBO_RELEASE_DATE,ULTIBO_RELEASE_NAME,ULTIBO_RELEASE_VERSION]));
 ClrScr;
 if InService then
  while True do
   Sleep(1);
end;

begin
 try
  Main;
 except on E:Exception do
  begin
   WriteLn(Format('Exception.Message %s',[E.Message]));
   LoggingOutput(Format('Exception.Message %s',[E.Message]));
   if InService then
    Sleep(5000);
  end;
 end;
 LoggingOutput('program stop');
end.
