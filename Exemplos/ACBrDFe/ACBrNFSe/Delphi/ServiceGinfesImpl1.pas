// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : C:\Documents and Settings\KIKO\Desktop\ServiceGinfesImpl.xml
// Codegen  : [wfUseSerializerClassForAttrs-]
// (27/04/2010 10:54:35 - - $Rev: 7300 $)
// ************************************************************************ //

unit ServiceGinfesImpl1;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Borland types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:string          - "http://www.w3.org/2001/XMLSchema"[]


  // ************************************************************************ //
  // Namespace : http://producao.ginfes.com.br
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : rpc
  // binding   : ServiceGinfesImplBinding
  // service   : ServiceGinfesImplService
  // port      : ServiceGinfesImplPort
  // URL       : https://producao.ginfes.com.br//ServiceGinfesImpl
  // ************************************************************************ //
  ServiceGinfesImpl = interface(IInvokable)
  ['{4E4F3EAA-E2FE-7E50-A061-F692B257D555}']
    function  CancelarNfse(const arg0: WideString): WideString; stdcall;
    function  CancelarNfseV3(const arg0: WideString; const arg1: WideString): WideString; stdcall;
    function  ConsultarLoteRps(const arg0: WideString): WideString; stdcall;
    function  ConsultarLoteRpsV3(const arg0: WideString; const arg1: WideString): WideString; stdcall;
    function  ConsultarNfse(const arg0: WideString): WideString; stdcall;
    function  ConsultarNfsePorRps(const arg0: WideString): WideString; stdcall;
    function  ConsultarNfsePorRpsV3(const arg0: WideString; const arg1: WideString): WideString; stdcall;
    function  ConsultarNfseV3(const arg0: WideString; const arg1: WideString): WideString; stdcall;
    function  ConsultarSituacaoLoteRps(const arg0: WideString): WideString; stdcall;
    function  ConsultarSituacaoLoteRpsV3(const arg0: WideString; const arg1: WideString): WideString; stdcall;
    function  RecepcionarLoteRps(const arg0: WideString): WideString; stdcall;
    function  RecepcionarLoteRpsV3(const arg0: WideString; const arg1: WideString): WideString; stdcall;
  end;

function GetServiceGinfesImpl(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): ServiceGinfesImpl;


implementation
  uses SysUtils;

function GetServiceGinfesImpl(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): ServiceGinfesImpl;
const
  defWSDL = 'ServiceGinfesImpl.xml';
  defURL  = 'https://producao.ginfes.com.br//ServiceGinfesImpl';
  defSvc  = 'ServiceGinfesImplService';
  defPrt  = 'ServiceGinfesImplPort';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as ServiceGinfesImpl);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


initialization
  InvRegistry.RegisterInterface(TypeInfo(ServiceGinfesImpl), 'http://producao.ginfes.com.br', '');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(ServiceGinfesImpl), '');

end.