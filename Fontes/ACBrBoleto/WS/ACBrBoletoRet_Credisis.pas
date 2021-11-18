
{$I ACBr.inc}

unit ACBrBoletoRet_Credisis;

interface

uses
  Classes, SysUtils, ACBrBoleto, ACBrBoletoWS, ACBrBoletoRetorno,  ACBrUtil, DateUtils, pcnConversao;

type

{ TRetornoEnvio_Credisis }

  TRetornoEnvio_Credisis = class(TRetornoEnvioSOAP)
  private

  public
    constructor Create(ABoletoWS: TACBrBoleto); override;
    destructor  Destroy; Override;
    function LerRetorno: Boolean;override;
    function RetornoEnvio: Boolean; override;

  end;

  const
  C_URL_Retorno = 'SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="urn:CredisisBoletoInterface-CredisisWebService"';

implementation

uses
  ACBrBoletoConversao;

{ TRetornoEnvio_Credisis }

constructor TRetornoEnvio_Credisis.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

destructor TRetornoEnvio_Credisis.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Credisis.LerRetorno: Boolean;
var
    RetornoCredisis: TRetEnvio;
    lXML: String;
begin
    Result := True;

    lXML:= StringReplace(Leitor.Arquivo, 'ns1:', '', [rfReplaceAll]) ;
    lXML:= StringReplace(lXML, C_URL_Retorno, '', [rfReplaceAll]) ;
    Leitor.Arquivo := lXML;
    Leitor.Grupo   := Leitor.Arquivo;

    RetornoCredisis:= ACBrBoleto.CriarRetornoWebNaLista;
    try
      if leitor.rExtrai(1, 'gerarBoletosResponse') <> '' then
      begin
          RetornoCredisis.DadosRet.ControleNegocial.NSU := Leitor.rCampo(tcStr, 'numeroSequencial');
          RetornoCredisis.DadosRet.IDBoleto.NossoNum    := Leitor.rCampo(tcStr, 'nossonumero');
          RetornoCredisis.DadosRet.IDBoleto.LinhaDig    := Leitor.rCampo(tcStr, 'linhaDigitavel');
          RetornoCredisis.DadosRet.IDBoleto.CodBarras   := Leitor.rCampo(tcStr, 'codigoBarras');

          RetornoCredisis.CodRetorno       := Leitor.rCampo(tcStr, 'code');
          RetornoCredisis.DadosRet.Excecao := Leitor.rCampo(tcStr, 'message');
      end;
    except
      Result := False;
    end;

  end;

function TRetornoEnvio_Credisis.RetornoEnvio: Boolean;
var
  lRetornoWS: String;
begin

  lRetornoWS := RetWS;
  RetWS := SeparaDados(lRetornoWS, 'SOAP-ENV:Body');

  Result:=inherited RetornoEnvio;

end;

end.

