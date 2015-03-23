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

//Leandro - Implementacao Link3 (UNIT CRIADA EM 30/10/2013 (COPIADO DO ACBrProvedorISSDigital.pas E ALTERADO))

unit ACBrProvedorLink3;

interface

uses
  Classes, SysUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorLink3 }

 TProvedorLink3 = class(TProvedorClass)
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

   function GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String; OverRide;
   function GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString; OverRide;

   function GeraRetornoNFSe(Prefixo: String; RetNFSe: AnsiString; NomeCidade: String): AnsiString; OverRide;
   function GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String; OverRide;
  end;

implementation

{ TProvedorLink3 }

constructor TProvedorLink3.Create;
begin
 {----}
end;

function TProvedorLink3.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := '';
  ConfigCidade.Identificador := 'Id';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://impl.nfse.services.l3grp.link3.com.br'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://impl.nfse.services.l3grp.link3.com.br';

  ConfigCidade.AssinaRPS   := True;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorLink3.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '2.00';
  ConfigSchema.VersaoDados           := '2.00';
  ConfigSchema.VersaoXML             := '2';
  ConfigSchema.NameSpaceXML          := 'http://www.abrasf.org.br/';
  ConfigSchema.Cabecalho             := 'nfse.xsd';
  ConfigSchema.ServicoEnviar         := 'nfse.xsd';
  ConfigSchema.ServicoConSit         := 'nfse.xsd';
  ConfigSchema.ServicoConLot         := 'nfse.xsd';
  ConfigSchema.ServicoConRps         := 'nfse.xsd';
  ConfigSchema.ServicoConNfse        := 'nfse.xsd';
  ConfigSchema.ServicoCancelar       := 'nfse.xsd';
  ConfigSchema.ServicoGerar          := 'nfse.xsd';
  ConfigSchema.ServicoEnviarSincrono := 'nfse.xsd';
  ConfigSchema.ServicoSubstituir     := 'nfse.xsd';
  ConfigSchema.DefTipos              := '';

  Result := ConfigSchema;
end;

function TProvedorLink3.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
  URL: String;
begin
  case ACodCidade of
   2911709: begin //GUANAMBI
              URL := 'http://webservice.guanambi.ba.link3.com.br:3189/nfseWebService';
            end;
  end;

  ConfigURL.HomRecepcaoLoteRPS    := URL;
  ConfigURL.HomConsultaLoteRPS    := URL;
  ConfigURL.HomConsultaNFSeRPS    := URL;
  ConfigURL.HomConsultaSitLoteRPS := URL;
  ConfigURL.HomConsultaNFSe       := URL;
  ConfigURL.HomCancelaNFSe        := URL;
  ConfigURL.HomGerarNFSe          := URL;
  ConfigURL.HomRecepcaoSincrono   := URL;
  ConfigURL.HomSubstituiNFSe      := URL;

  ConfigURL.ProRecepcaoLoteRPS    := URL;
  ConfigURL.ProConsultaLoteRPS    := URL;
  ConfigURL.ProConsultaNFSeRPS    := URL;
  ConfigURL.ProConsultaSitLoteRPS := URL;
  ConfigURL.ProConsultaNFSe       := URL;
  ConfigURL.ProCancelaNFSe        := URL;
  ConfigURL.ProGerarNFSe          := URL;
  ConfigURL.ProRecepcaoSincrono   := URL;
  ConfigURL.ProSubstituiNFSe      := URL;

  Result := ConfigURL;
end;

function TProvedorLink3.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorLink3.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 Result := False;
 case Acao of
   acRecepcionar: Result := True;
   acConsSit:     Result := False;
   acConsLote:    Result := False;
   acConsNFSeRps: Result := False;
   acConsNFSe:    Result := False;
   acCancelar:    Result := True;
   acGerar:       Result := False;
   acRecSincrono: Result := True;
   acSubstituir:  Result := False;
 end;
end;

function TProvedorLink3.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorLink3.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseServicoPrestadoEnvio' + NameSpaceDad;
   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'Pedido>' +
                              '<' + Prefixo4 + 'InfPedidoCancelamento';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  SeSenao(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorLink3.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho' +
            ' versao="'  + VersaoLayOut + '"' +
            ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
            ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorLink3.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorLink3.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseServicoPrestadoEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorLink3.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
// DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<tns:recepcionarLoteRps xmlns:tns="' + URLNS + '/">' +
                DadosMsg +
             '</tns:recepcionarLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorLink3.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

// DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<tns:consultarSituacaoLoteRps xmlns:tns="' + URLNS + '/">' +
                DadosMsg +
             '</tns:consultarSituacaoLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorLink3.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

// DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<tns:consultarLoteRps xmlns:tns="' + URLNS + '/">' +
                DadosMsg +
             '</tns:consultarLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorLink3.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

// DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<tns:consultarNfsePorRps xmlns:tns="' + URLNS + '/">' +
                DadosMsg +
             '</tns:consultarNfsePorRps>' +
            '</s:Body>' +
           '</s:Envelope>';

end;

function TProvedorLink3.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

// DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<tns:consultarNfse xmlns:tns="' + URLNS + '/">' +
                DadosMsg +
             '</tns:consultarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';

end;

function TProvedorLink3.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

// DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
// DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<tns:cancelarNfse xmlns:tns="' + URLNS + '/">' +
                DadosMsg +
             '</tns:cancelarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorLink3.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<tns:gerarNfse xmlns:tns="' + URLNS + '/">' +
                DadosMsg +
             '</tns:gerarNfse>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorLink3.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<tns:recepcionarLoteRpsSincrono xmlns:tns="' + URLNS + '/">' +
                DadosMsg +
             '</tns:recepcionarLoteRpsSincrono>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorLink3.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorLink3.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'recepcionarLoteRps';
   acConsSit:     Result := 'consultarSituacaoLoteRps';
   acConsLote:    Result := 'consultarLoteRps';
   acConsNFSeRps: Result := 'consultarNfsePorRps';
   acConsNFSe:    Result := 'consultarNfse';
   acCancelar:    Result := 'cancelarNfse';
   acGerar:       Result := 'gerarNfse';
   acRecSincrono: Result := 'recepcionarLoteRpsSincrono';
   acSubstituir:  Result := '';
 end;;
end;

function TProvedorLink3.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'tns:recepcionarLoteRpsResponse' );
   acConsSit:     Result := SeparaDados( RetornoWS, 'tns:consultarSituacaoLoteRpsResponse' );
   acConsLote:    Result := SeparaDados( RetornoWS, 'tns:consultarLoteRpsResponse' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'tns:consultarNfsePorRpsResponse' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'tns:consultarNfseResponse' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'tns:cancelarNfseResponse' );
   acGerar:       Result := SeparaDados( RetornoWS, 'tns:gerarNfseResponse' );
   acRecSincrono: Result := SeparaDados( RetornoWS, 'tns:recepcionarLoteRpsSincronoResponse' );
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorLink3.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/nfse.xsd">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorLink3.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 if AAmbiente = 1
  then begin
   case ACodMunicipio of
    2911709: Result := '';
   else Result := '';
   end;
  end
  else Result := '';
end;

end.
