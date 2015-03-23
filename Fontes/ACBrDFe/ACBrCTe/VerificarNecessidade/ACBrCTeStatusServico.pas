// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : C:\CteStatusServico.wsdl
// Encoding : UTF-8
// Version  : 1.0
// (21/8/2009 16:56:03 - 1.33.2.5)
// ************************************************************************ //

unit ACbrCteStatusServico;

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
  // !:string          - "http://www.w3.org/2001/XMLSchema"



  // ************************************************************************ //
  // Namespace : http://www.portalfiscal.inf.br/cte/wsdl/CteStatusServico
  // binding   : CteStatusServicoSoap12
  // service   : CteStatusServico
  // port      : CteStatusServico
  // ************************************************************************ //
  CteStatusServicoSoap12 = interface(IInvokable)
  ['{73C5E5F1-8AFD-41BC-F3B9-8B7C098CE4D9}']
    function  cteStatusServicoCT(const cteDadosMsg: WideString): WideString; stdcall;
  end;

function GetCteStatusServicoSoap12(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): CteStatusServicoSoap12;


implementation

function GetCteStatusServicoSoap12(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): CteStatusServicoSoap12;
const
  defWSDL = 'CteStatusServico.wsdl';
  defURL  = '';
  defSvc  = 'CteStatusServico';
  defPrt  = 'CteStatusServico';
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
    Result := (RIO as CteStatusServicoSoap12);
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
  InvRegistry.RegisterInterface(TypeInfo(CteStatusServicoSoap12), 'http://www.portalfiscal.inf.br/cte/wsdl/CteStatusServico', 'UTF-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(CteStatusServicoSoap12), 'http://www.portalfiscal.inf.br/cte/wsdl/CteStatusServico/cteStatusServicoCT');
  InvRegistry.RegisterInvokeOptions(TypeInfo(CteStatusServicoSoap12), ioDocument);
end.
