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

unit ACBrProvedor4R;

interface

uses
  Classes, SysUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedor4R }

 TProvedor4R = class(TProvedorClass)
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

{ TProvedor4R }
{ Informações de apoio: http://www.4rsistemas.com.br/wpprodutosdetalhes.aspx?24 }

constructor TProvedor4R.Create;
begin
 {----}
end;

function TProvedor4R.GetConfigCidade(ACodCidade, AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
 	ConfigCidade.VersaoSoap        := '1.1';
 	ConfigCidade.Prefixo2          := '';
 	ConfigCidade.Prefixo3          := '';
 	ConfigCidade.Prefixo4          := '';
 	ConfigCidade.Identificador     := 'Id';
  ConfigCidade.QuebradeLinha     := ';';
	ConfigCidade.NameSpaceEnvelope := 'http://www.abrasf.org.br';

 	ConfigCidade.AssinaRPS   := True;
 	ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

 	Result := ConfigCidade;
end;

function TProvedor4R.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
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

function TProvedor4R.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 	ConfigURL: TConfigURL;
begin
  case ACodCidade of
//   3127701:
//     begin
//         ConfigURL.ProNomeCidade := 'valadares';
//         ConfigURL.HomNomeCidade := 'valadares';
//	   end;
   3500105: begin
              ConfigURL.ProNomeCidade := 'adamantina';
              ConfigURL.HomNomeCidade := 'adamantina';
            end;
   3510203: begin
    	        ConfigURL.ProNomeCidade := 'capaobonito';
              ConfigURL.HomNomeCidade := 'capaobonito';
   	        end;
   3522109: begin
	            ConfigURL.ProNomeCidade := 'itanhaem';
              ConfigURL.HomNomeCidade := 'itanhaem';
	          end;
   3523503: begin
	            ConfigURL.ProNomeCidade := 'itatinga';
              ConfigURL.HomNomeCidade := 'itatinga';
	          end;
   3554003: begin
	            ConfigURL.ProNomeCidade := 'tatui';
              ConfigURL.HomNomeCidade := 'tatui';
	          end;
   3555109: begin
              ConfigURL.ProNomeCidade := 'tupipaulista';
              ConfigURL.HomNomeCidade := 'tupipaulista';
            end;
  end;

  ConfigURL.HomRecepcaoLoteRPS    := '';
  ConfigURL.HomConsultaLoteRPS    := '';
  ConfigURL.HomConsultaNFSeRPS    := Format('http://%s.sistemas4r.com.br/abrasf/ahconsultarnfseporrps.aspx?wsdl',[ConfigURL.HomNomeCidade]);
  ConfigURL.HomConsultaSitLoteRPS := '';
  ConfigURL.HomConsultaNFSe       := '';
  ConfigURL.HomCancelaNFSe        := Format('http://%s.sistemas4r.com.br/abrasf/ahcancelarnfse.aspx?wsdl',[ConfigURL.HomNomeCidade]);
  ConfigURL.HomGerarNFSe          := '';
  ConfigURL.HomRecepcaoSincrono   := Format('http://%s.sistemas4r.com.br/abrasf/ahrecepcionarloterpssincrono.aspx?wsdl',[ConfigURL.HomNomeCidade]);
  ConfigURL.HomSubstituiNFSe      := '';

  ConfigURL.ProRecepcaoLoteRPS    := '';
  ConfigURL.ProConsultaLoteRPS    :=Format('http://%s.sistemas4r.com.br/abrasf/aconsultarloterps.aspx?wsdl',[ConfigURL.ProNomeCidade]);
  //'';
  ConfigURL.ProConsultaNFSeRPS    := Format('http://%s.sistemas4r.com.br/abrasf/aconsultarnfseporrps.aspx?wsdl',[ConfigURL.ProNomeCidade]);
  ConfigURL.ProConsultaSitLoteRPS := '';
  ConfigURL.ProConsultaNFSe       := '';
  ConfigURL.ProCancelaNFSe        := Format('http://%s.sistemas4r.com.br/abrasf/acancelarnfse.aspx?wsdl',[ConfigURL.ProNomeCidade]);
  ConfigURL.ProGerarNFSe          := '';
  ConfigURL.ProRecepcaoSincrono   := Format('http://%s.sistemas4r.com.br/abrasf/arecepcionarloterpssincrono.aspx?wsdl',[ConfigURL.ProNomeCidade]);
  ConfigURL.ProSubstituiNFSe      := '';

  Result := ConfigURL;
end;

function TProvedor4R.GetURI(URI: String): String;
begin
  Result := URI;
end;

function TProvedor4R.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := False;
   acConsSit:     Result := False;
   acConsLote:    Result := False;
   acConsNFSeRps: Result := False;
   acConsNFSe:    Result := False;
   acCancelar:    Result := False;
   acGerar:       Result := False;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
   acConsSecRps:  Result := False;
 end;
end;

function TProvedor4R.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedor4R.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRps' + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseEnvio' + NameSpaceDad;
   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'Pedido>' +
                              '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                 SeSenao(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  SeSenao(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedor4R.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedor4R.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedor4R.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRps>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedor4R.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
end;

function TProvedor4R.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
end;

function TProvedor4R.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                        'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                        'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<ConsultarLoteRps.Execute xmlns="http://tempuri.org/">' +
              '<Entrada>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</Entrada>' +
             '</ConsultarLoteRps.Execute>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedor4R.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                        'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                        'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<ConsultarNfsePorRps.Execute xmlns="Abrasf2">' +
              '<Entrada>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</Entrada>' +
             '</ConsultarNfsePorRps.Execute>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedor4R.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
end;

function TProvedor4R.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<CancelarNfse.Execute xmlns="Abrasf2">' +
              '<Entrada>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</Entrada>' +
             '</CancelarNfse.Execute>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedor4R.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<GerarNfse.Execute xmlns="http://tempuri.org/">' +
              '<Entrada>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</Entrada>' +
             '</GerarNfse.Execute>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedor4R.GeraEnvelopeRecepcionarSincrono(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             // Incluido por João Paulo Delboni em 22/04/2013
             '<RecepcionarLoteRpsSincrono.Execute xmlns="Abrasf2">' +
              '<Entrada>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</Entrada>' +
             '</RecepcionarLoteRpsSincrono.Execute>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedor4R.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedor4R.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '';
   acConsLote:    Result := 'http://tempuri.org/action/ACONSULTARLOTERPS.Execute';
   acConsNFSeRps: Result := 'Abrasf2action/ACONSULTARNFSEPORRPS.Execute';
   acConsNFSe:    Result := '';
   acCancelar:    Result := 'Abrasf2action/ACANCELARNFSE.Execute';
   acGerar:       Result := 'http://tempuri.org/action/AGERARNFSE.Execute';
   acRecSincrono: Result := 'Abrasf2action/ARECEPCIONARLOTERPSSINCRONO.Execute';
   acSubstituir:  Result := '';
 end;
end;

function TProvedor4R.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := RetornoWS;
   acConsSit:     Result := RetornoWS;
   acConsLote:    Result := SeparaDados( RetornoWS, 'Resposta' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'Resposta' );
   acConsNFSe:    Result := RetornoWS;
   acCancelar:    Result := SeparaDados( RetornoWS, 'Resposta' );
   acGerar:       Result := SeparaDados( RetornoWS, 'Resposta' );
   acRecSincrono: Result := SeparaDados( RetornoWS, 'Resposta' );
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedor4R.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<CompNfse xmlns="http://www.abrasf.org.br/nfse.xsd">' +
            RetNFSe +
           '</CompNfse>';
end;

function TProvedor4R.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
var
 NomeMunicipio: String;
begin
 case ACodMunicipio of
   3127701: NomeMunicipio := 'valadares';
   3500105: NomeMunicipio := 'adamantina';
   3510203: NomeMunicipio := 'capaobonito';
   3522109: NomeMunicipio := 'itanhaem';
   3523503: NomeMunicipio := 'itatinga';
   3554003: NomeMunicipio := 'tatui';
 else
   NomeMunicipio := '';
 end;

 if AAmbiente = 1 then
   Result := 'https://' + NomeMunicipio + '.sistemas4r.com.br/CS/Em_Impressao_Nfe.aspx?id=' + ACodVerificacao
 else
   Result := 'https://' + NomeMunicipio + '.sistemas4r.com.br/CS/Em_Impressao_NfeHomologa.aspx?id=' + ACodVerificacao;

end;

end.
