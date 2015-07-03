{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrProvedorCTA;

interface

uses
  Classes, SysUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorCTA }

 TProvedorCTA = class(TProvedorClass)
  protected
   { protected }
  private
   { private }
  public
   { public }
   Constructor Create;

   function GetConfigCidade(ACodCidade, AAmbiente: Integer): TConfigCidade; OverRide;
   function GetConfigSchema(ACodCidade: Integer): TConfigSchema; OverRide;
   function GetConfigURL(ACodCidade: Integer): TConfigURL; OverRide;
   function GetURI(URI: String): String; OverRide;
   function GetAssinarXML(Acao: TnfseAcao): Boolean; OverRide;
   function GetValidarLote: Boolean; OverRide;

   function Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4, NameSpaceDad, Identificador, URI: String): AnsiString; OverRide;
   function Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados, NameSpaceCab: String; ACodCidade: Integer): AnsiString; OverRide;
   function Gera_DadosSenha(CNPJ, Senha: String): AnsiString; OverRide;
   function Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString; OverRide;

   function GeraEnvelopeRecepcionarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarSituacaoLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarNFSeporRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeGerarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeRecepcionarSincrono(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;

   function GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;

   function GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String; OverRide;
   function GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString; OverRide;

   function GeraRetornoNFSe(Prefixo: String; RetNFSe: AnsiString; NomeCidade: String): AnsiString; OverRide;
   function GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String; OverRide;
  end;

implementation

{ TProvedorCTA }

constructor TProvedorCTA.Create;
begin
 {----}
end;

function TProvedorCTA.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := 'ns1:';
  ConfigCidade.Prefixo4      := 'tipos:';
  ConfigCidade.Identificador := 'Id';
  ConfigCidade.QuebradeLinha := ';';

  ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := (AAmbiente = 1);
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorCTA.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho := '';
  ConfigSchema.VersaoDados     := '';
  ConfigSchema.VersaoXML       := '1';
  ConfigSchema.NameSpaceXML    := 'http://localhost:8080/WsNFe2/lote'; 
                                   
  ConfigSchema.Cabecalho             := '';
  ConfigSchema.ServicoEnviar         := 'ReqEnvioLoteRPS.xsd';
  ConfigSchema.ServicoConSit         := 'ConsultaSeqRps.xsd';
  ConfigSchema.ServicoConLot         := 'ReqConsultaLote.xsd';
  ConfigSchema.ServicoConRps         := 'ReqConsultaNFSeRPS.xsd';
  ConfigSchema.ServicoConNfse        := 'ReqConsultaNotas.xsd';
  ConfigSchema.ServicoCancelar       := 'ReqCancelamentoNFSe.xsd';
  ConfigSchema.ServicoConSeqRps      := 'consultarSequencialRps.xsd';
  ConfigSchema.ServicoGerar          := '';
  ConfigSchema.ServicoEnviarSincrono := '';
  ConfigSchema.ServicoSubstituir     := '';
  ConfigSchema.DefTipos              := ''; 

  Result := ConfigSchema;
end;

function TProvedorCTA.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
begin
  ConfigURL.HomNomeCidade         := '';
  ConfigURL.ProNomeCidade         := '';

  case ACodCidade of
    2111300: // Sao Luis/MA
      begin
        ConfigURL.HomRecepcaoLoteRPS    := 'http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps?wsdl';
        ConfigURL.ProRecepcaoLoteRPS    := 'http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps?wsdl';
      end;
  end;

  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSeqRPS     := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProConsultaLoteRPS    := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaNFSeRPS    := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaSitLoteRPS := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaNFSe       := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProCancelaNFSe        := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaSeqRPS     := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProGerarNFSe          := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProRecepcaoSincrono   := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProSubstituiNFSe      := ConfigURL.ProRecepcaoLoteRPS;

  Result := ConfigURL;
end;

function TProvedorCTA.GetURI(URI: String): String;
begin
  Result := '';
end;

function TProvedorCTA.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := True;
   acConsSit:     Result := False;
   acConsLote:    Result := False;
   acConsNFSeRps: Result := True;
   acConsNFSe:    Result := True;
   acCancelar:    Result := True;
   acGerar:       Result := False;
   acConsSecRps:  Result := True;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
 end;
end;

function TProvedorCTA.GetValidarLote: Boolean;
begin
  Result := False; //Nao tem schemas
end;

function TProvedorCTA.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
    acRecepcionar: Result := '<' + Prefixo3 + 'ReqEnvioLoteRPS' + NameSpaceDad;
    acConsSit:     Result := '';
    acConsLote:    Result := '<' + Prefixo3 + 'ReqConsultaLote' + NameSpaceDad;
    acConsNFSeRps: Result := '<' + Prefixo3 + 'ReqConsultaNFSeRPS' + NameSpaceDad;
    acConsNFSe:    Result := '<' + Prefixo3 + 'ReqConsultaNotas' + NameSpaceDad;
    acCancelar:    Result := '<' + Prefixo3 + 'ReqCancelamentoNFSe' + NameSpaceDad;
    acGerar:       Result := '<' + Prefixo3 + 'ReqEnvioLoteRPS' + NameSpaceDad;
    acConsSecRps:  Result := '<' + Prefixo3 + 'ConsultaSeqRps' + NameSpaceDad;
    acRecSincrono: Result := '';
    acSubstituir:  Result := '';
  end;
end;

function TProvedorCTA.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
  Result := '';
end;

function TProvedorCTA.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
  Result := '';
end;

function TProvedorCTA.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
    acRecepcionar: Result := '</' + Prefixo3 + 'ReqEnvioLoteRPS>';
    acConsSit:     Result := '';
    acConsLote:    Result := '</' + Prefixo3 + 'ReqConsultaLote>';
    acConsNFSeRps: Result := '</' + Prefixo3 + 'ReqConsultaNFSeRPS>';
    acConsNFSe:    Result := '</' + Prefixo3 + 'ReqConsultaNotas>';
    acCancelar:    Result := '</' + Prefixo3 + 'ReqCancelamentoNFSe>';
    acGerar:       Result := '</' + Prefixo3 + 'ReqEnvioLoteRPS>';
    acConsSecRps:  Result := '</' + Prefixo3 + 'ConsultaSeqRps>';
    acRecSincrono: Result := '';
    acSubstituir:  Result := '';
  end;
end;

function TProvedorCTA.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result:= '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:lot="http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps.jws">' +
             '<soapenv:Header/>' +
             '<soapenv:Body>' +
               '<lot:enviar>' +
                 '<mensagemXml>' +
                   StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                 '</mensagemXml>' +
               '</lot:enviar>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorCTA.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorCTA.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result:= '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:lot="http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps.jws">' +
             '<soapenv:Header/>' +
             '<soapenv:Body>' +
               '<lot:consultarLote>' +
                 '<mensagemXml>' +
                   StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                 '</mensagemXml>' +
               '</lot:consultarLote>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorCTA.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result:= '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:lot="http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps.jws">' +
             '<soapenv:Header/>' + 
             '<soapenv:Body>' +
               '<lot:consultarNFSeRps>' +
                 '<mensagemXml>' +
                   StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                 '</mensagemXml>' +
               '</lot:consultarNFSeRps>' +
            '</soapenv:Body>' +
          '</soapenv:Envelope>';
end;

function TProvedorCTA.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result:= '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:lot="http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps.jws">' +
             '<soapenv:Header/>' +
             '<soapenv:Body>' +
               '<lot:consultarNota>' +
                  '<mensagemXml>' +
                    StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                  '</mensagemXml>' + 
               '</lot:consultarNota>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorCTA.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result:= '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
           ' xmlns:lot="http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps.jws">' +
             '<soapenv:Header/>' +
             '<soapenv:Body>' +
               '<lot:cancelar>' +
                  '<mensagemXml>' +
                    StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                  '</mensagemXml>' +
               '</lot:cancelar>' +
            '</soapenv:Body>' +
          '</soapenv:Envelope>';
end;

function TProvedorCTA.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorCTA.GeraEnvelopeRecepcionarSincrono(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorCTA.GeraEnvelopeSubstituirNFSe(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorCTA.GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result:= '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:lot="http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps.jws">' +
             '<soapenv:Header/>' +
             '<soapenv:Body>' +
               '<lot:consultarSequencialRps>' +
                  '<mensagemXml>' +
                    StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                  '</mensagemXml>' +
               '</lot:consultarSequencialRps>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorCTA.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
  case Acao of
    acRecepcionar: Result := 'enviar';
    acConsSit:     Result := '';
    acConsLote:    Result := 'consultarLote';
    acConsNFSeRps: Result := 'consultarNFSeRps';
    acConsNFSe:    Result := 'consultarNota';
    acCancelar:    Result := 'cancelar';
    acGerar:       Result := '';
    acRecSincrono: Result := 'enviarSincrono';
    acConsSecRps:  Result := 'consultarSequencialRps';
    acSubstituir:  Result := '';
  end;
end;

function TProvedorCTA.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
var
  PosIni, PosFim: Integer;
  Tag: String;
begin
{case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'ns1:RetornoEnvioLoteRPS', True );
   acConsSit:     Result := RetornoWS;
   acConsLote:    Result := SeparaDados( RetornoWS, 'ns1:RetornoConsultaLote', True );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'ns1:RetornoConsultaNFSeRPS', True );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'ns1:RetornoConsultaNotas', True );
   acCancelar:    Result := SeparaDados( RetornoWS, 'ns1:RetornoCancelamentoNFSe', True );
   acGerar:       Result := RetornoWS;
   acConsSecRps:  Result := SeparaDados( RetornoWS, 'ns1:RetornoConsultaSeqRps', True );
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;}

  Tag:= '';
  case Acao of
    acRecepcionar: Tag    := 'ns1:RetornoEnvioLoteRPS';
    acConsSit:     Result := RetornoWS;
    acConsLote:    Tag    := 'ns1:RetornoConsultaLote';
    acConsNFSeRps: Tag    := 'ns1:RetornoConsultaNFSeRPS';
    acConsNFSe:    Tag    := 'ns1:RetornoConsultaNotas';
    acCancelar:    Tag    := 'ns1:RetornoCancelamentoNFSe';
    acGerar:       Result := RetornoWS;
    acConsSecRps:  Tag    := 'ns1:RetornoConsultaSeqRps';
    acRecSincrono: Result := RetornoWS;
    acSubstituir:  Result := RetornoWS;
  end;

  if (Tag<>'') then
    begin
      if (Pos(Tag, RetornoWS) <= 0) then 
        begin		  
		  //Tem método q vem com prefixo, tem método q não
          Tag    := Copy(Tag, 5, MaxInt); 		  
		  //Não usei a função SeparaDados, pois quando não tem prefixo, o nome tag no xml repete no schemaLocation
		  //Copiei o código da função SeparaDados e no PosFim coloquei "</"
          PosIni := Pos('<' + Tag, RetornoWS);
          PosFim := Pos('</' + Tag, RetornoWS) + length(Tag) + 4;
          Result := AnsiString(Copy(String(RetornoWS), PosIni, PosFim - (PosIni + 1)));
        end
      else
        Result := SeparaDados( RetornoWS, Tag, True );
    end;
end;

function TProvedorCTA.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
  Result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<CompNfse xmlns:ns4="http://www.e-governeapps2.com.br/nfse.xsd">' +
              RetNFSe +
            '</CompNfse>';
end;

function TProvedorCTA.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
  Result := '';
end;

end.
