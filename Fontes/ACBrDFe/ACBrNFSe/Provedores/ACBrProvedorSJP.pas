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

unit ACBrProvedorSJP;

interface

uses
  Classes, SysUtils, pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorSJP }

 TProvedorSJP = class(TProvedorClass)
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

{ TProvedorSJP }

constructor TProvedorSJP.Create;
begin
 {----}
end;

function TProvedorSJP.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := 'ns2:';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := 'tipos:';
  ConfigCidade.Identificador := 'Id';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1
   then ConfigCidade.NameSpaceEnvelope := 'http://nfe.sjp.pr.gov.br'
   else ConfigCidade.NameSpaceEnvelope := 'http://nfe.sjp.pr.gov.br';

  ConfigCidade.AssinaRPS   := True;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorSJP.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho := '3';
  ConfigSchema.VersaoDados     := '3';
  ConfigSchema.VersaoXML       := '1';
  ConfigSchema.NameSpaceXML    := 'http://nfe.sjp.pr.gov.br/';
  ConfigSchema.Cabecalho       := 'cabecalho_v03.xsd';
  ConfigSchema.ServicoEnviar   := 'servico_enviar_lote_rps_envio_v03.xsd';
  ConfigSchema.ServicoConSit   := 'servico_consultar_situacao_lote_rps_envio_v03.xsd';
  ConfigSchema.ServicoConLot   := 'servico_consultar_lote_rps_envio_v03.xsd';
  ConfigSchema.ServicoConRps   := 'servico_consultar_nfse_rps_envio_v03.xsd';
  ConfigSchema.ServicoConNfse  := 'servico_consultar_nfse_envio_v03.xsd';
  ConfigSchema.ServicoCancelar := 'servico_cancelar_nfse_envio_v03.xsd';

  ConfigSchema.ServicoGerar          := '';
  ConfigSchema.ServicoEnviarSincrono := '';
  ConfigSchema.ServicoSubstituir     := '';
  ConfigSchema.DefTipos              := 'tipos_v03.xsd';

  Result := ConfigSchema;
end;

function TProvedorSJP.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
begin
  ConfigURL.HomNomeCidade         := '';
  ConfigURL.HomRecepcaoLoteRPS    := 'https://nfe.sjp.pr.gov.br/servicos/issOnline2/homologacao/ws/index.php';

  ConfigURL.ProNomeCidade         := '';
  ConfigURL.ProRecepcaoLoteRPS    := 'https://nfe.sjp.pr.gov.br/servicos/issOnline2/ws/index.php';

  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProConsultaLoteRPS    := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaNFSeRPS    := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaSitLoteRPS := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaNFSe       := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProCancelaNFSe        := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProGerarNFSe          := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProRecepcaoSincrono   := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProSubstituiNFSe      := ConfigURL.ProRecepcaoLoteRPS;

  Result := ConfigURL;
end;

function TProvedorSJP.GetURI(URI: String): String;
begin
 // No provedor SJP a URI não é informada.
 Result := '';
end;

function TProvedorSJP.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := True;
   acConsSit:     Result := True;
   acConsLote:    Result := True;
   acConsNFSeRps: Result := True;
   acConsNFSe:    Result := True;
   acCancelar:    Result := True;
   acGerar:       Result := False;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
   acConsSecRps:  Result := False;
 end;
end;

function TProvedorSJP.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorSJP.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseEnvio' + NameSpaceDad;
   acCancelar:    Result := '<CancelarNfseEnvio' +
                               ' xmlns="http://nfe.sjp.pr.gov.br/servico_cancelar_nfse_envio"' +
                               ' xmlns:' + stringReplace(Prefixo4, ':', '', []) + '="http://nfe.sjp.pr.gov.br/tipos">';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  DFeUtil.SeSenao(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorSJP.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
  Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
             '<versaoDados>' + VersaoDados + '</versaoDados>'+
            '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorSJP.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorSJP.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar:    Result := '</CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorSJP.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
var
 TagCab, TagDados: String;
begin
  TagCab   := 'arg0';
  TagDados := 'arg1';
  CabMsg   := '<![CDATA[' + CabMsg + ']]>';
  DadosMsg := '<![CDATA[' + DadosMsg + ']]>';

  Result := '<?xml version="1.0"?>' +
            '<soapenv:Envelope xmlns:nfe="http://nfe.sjp.pr.gov.br" ' +
                              'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">' +
             '<soapenv:Header/>' +
             '<soapenv:Body>' +
              '<nfe:RecepcionarLoteRpsV3>' +
               '<' + TagCab + '>' + CabMsg + '</' + TagCab + '>' +
               '<' + TagDados + '>' + DadosMsg + '</' + TagDados + '>' +
               {
               '<arg0>' +
                '<![CDATA[<ns2:cabecalho versao="3" xmlns:ns2="http://nfe.sjp.pr.gov.br/cabecalho_v03.xsd"><versaoDados>3</versaoDados></ns2:cabecalho>]]>' +
               '</arg0>' +
               '<arg1>' +
               '</arg1>' +
               }
              '</nfe:RecepcionarLoteRpsV3>' +
             '</soapenv:Body>' +
            '</soapenv:Envelope>';


 {
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ns1:RecepcionarLoteRpsV3 xmlns:ns1="' + URLNS + '">' +
              '<' + TagCab + '>' + CabMsg + '</' + TagCab + '>' +
              '<' + TagDados + '>' + DadosMsg + '</' + TagDados + '>' +
             '</ns1:RecepcionarLoteRpsV3>' +
            '</s:Body>' +
           '</s:Envelope>';
 }
end;

function TProvedorSJP.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
var
 TagCab, TagDados: String;
begin
 if Pos('fortaleza', URLNS) > 0
  then begin
   TagCab   := 'Cabecalho';
   TagDados := 'ConsultarSituacaoLoteRpsEnvio';
   CabMsg   := '<![CDATA[' + CabMsg + ']]>';
   DadosMsg := '<![CDATA[' + DadosMsg + ']]>';
  end
  else begin
   TagCab   := 'arg0';
   TagDados := 'arg1';
  end;

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:ns1="' + URLNS + '">' +
            '<s:Body>' +
             '<ns1:ConsultarSituacaoLoteRpsV3>' +
              '<' + TagCab + '>' + CabMsg + '</' + TagCab + '>' +
              '<' + TagDados + '>' + DadosMsg + '</' + TagDados + '>' +
             '</ns1:ConsultarSituacaoLoteRpsV3>' +
            '</s:Body>' +
           '</s:Envelope>';
(*
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ns1:ConsultarSituacaoLoteRpsV3 xmlns:ns1="' + URLNS + '">' +
              '<' + TagCab + '>' + CabMsg + '</' + TagCab + '>' +
              '<' + TagDados + '>' + DadosMsg + '</' + TagDados + '>' +
             '</ns1:ConsultarSituacaoLoteRpsV3>' +
            '</s:Body>' +
           '</s:Envelope>';
*)
end;

function TProvedorSJP.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
var
 TagCab, TagDados: String;
begin
 if Pos('fortaleza', URLNS) > 0
  then begin
   TagCab   := 'Cabecalho';
   TagDados := 'ConsultarLoteRpsEnvio';
   CabMsg   := '<![CDATA[' + CabMsg + ']]>';
   DadosMsg := '<![CDATA[' + DadosMsg + ']]>';
  end
  else begin
   TagCab   := 'arg0';
   TagDados := 'arg1';
  end;

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ns1:ConsultarLoteRpsV3 xmlns:ns1="' + URLNS + '">' +
              '<' + TagCab + '>' + CabMsg + '</' + TagCab + '>' +
              '<' + TagDados + '>' + DadosMsg + '</' + TagDados + '>' +
             '</ns1:ConsultarLoteRpsV3>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSJP.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
var
 TagCab, TagDados: String;
begin
 if Pos('fortaleza', URLNS) > 0
  then begin
   TagCab   := 'Cabecalho';
   TagDados := 'ConsultarNfseRpsEnvio';
   CabMsg   := '<![CDATA[' + CabMsg + ']]>';
   DadosMsg := '<![CDATA[' + DadosMsg + ']]>';
  end
  else begin
   TagCab   := 'arg0';
   TagDados := 'arg1';
  end;

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ns1:ConsultarNfsePorRpsV3 xmlns:ns1="' + URLNS + '">' +
              '<' + TagCab + '>' + CabMsg + '</' + TagCab + '>' +
              '<' + TagDados + '>' + DadosMsg + '</' + TagDados + '>' +
             '</ns1:ConsultarNfsePorRpsV3>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSJP.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
var
 TagCab, TagDados: String;
begin
 if Pos('fortaleza', URLNS) > 0
  then begin
   TagCab   := 'Cabecalho';
   TagDados := 'ConsultarNfseEnvio';
   CabMsg   := '<![CDATA[' + CabMsg + ']]>';
   DadosMsg := '<![CDATA[' + DadosMsg + ']]>';
  end
  else begin
   TagCab   := 'arg0';
   TagDados := 'arg1';
  end;

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ns1:ConsultarNfseV3 xmlns:ns1="' + URLNS + '">' +
              '<' + TagCab + '>' + CabMsg + '</' + TagCab + '>' +
              '<' + TagDados + '>' + DadosMsg + '</' + TagDados + '>' +
             '</ns1:ConsultarNfseV3>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSJP.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
var
 TagDados: String;
begin
 if Pos('fortaleza', URLNS) > 0
  then begin
   TagDados := 'CancelarNfseEnvio';
   DadosMsg := '<![CDATA[' + DadosMsg + ']]>';
  end
  else begin
   TagDados := 'arg0';
   DadosMsg := StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
  end;

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ns1:CancelarNfse xmlns:ns1="' + URLNS + '">' +
              '<' + TagDados +'>' +
               '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
               DadosMsg +
              '</' + TagDados +'>' +
             '</ns1:CancelarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSJP.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSJP.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSJP.GeraEnvelopeSubstituirNFSe(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSJP.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '';
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := '';
   acCancelar:    Result := '';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorSJP.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: begin
                   Result := SeparaDados( RetornoWS, 'return' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'EnviarLoteRpsResposta' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'soap:Body' )
                    else Result := Result + '</EnviarLoteRpsResposta>';
                  end;
   acConsSit:     begin
                   Result := SeparaDados( RetornoWS, 'return' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'ConsultarSituacaoLoteRpsResposta' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'soap:Body' )
                    else Result := Result + '</ConsultarSituacaoLoteRpsResposta>';
                  end;
   acConsLote:    begin
                   Result := SeparaDados( RetornoWS, 'return' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'ConsultarLoteRpsResposta' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'soap:Body' )
                    else Result := Result + '</ConsultarLoteRpsResposta>';
                  end;
   acConsNFSeRps: begin
                   Result := SeparaDados( RetornoWS, 'return' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'ConsultarNfsePorRpsResposta' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'soap:Body' )
                    else Result := Result + '</ConsultarNfsePorRpsResposta>';
                  end;
   acConsNFSe:    begin
                   Result := SeparaDados( RetornoWS, 'return' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'ConsultarNfseResposta' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'soap:Body' )
                    else Result := Result + '</ConsultarNfseResposta>';
                  end;
   acCancelar:    begin
                   Result := SeparaDados( RetornoWS, 'return' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'CancelarNfseResposta' );
                   if Result = ''
                    then Result := SeparaDados( RetornoWS, 'soap:Body' )
                    else Result := Result + '</CancelarNfseResposta>';
                  end;
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorSJP.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<CompNfse xmlns:ns4="http://nfe.sjp.pr.gov.br/tipos_v03.xsd">' +
            RetNFSe +
           '</CompNfse>';
end;

function TProvedorSJP.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 if AAmbiente = 1 then
   Result := ''
//             'http://visualizar.ginfes.com.br/report/consultarNota?__report=nfs_ver4&cdVerificacao=' +
//             ACodVerificacao + '&numNota=' + IntToStr(ANumeroNFSe) + '&cnpjPrestador=null'
 else
   Result := '';
//             http://visualizar.ginfesh.com.br/report/consultarNota?__report=nfs_ver4&cdVerificacao=' +
//             ACodVerificacao + '&numNota=' + IntToStr(ANumeroNFSe) + '&cnpjPrestador=null';
end;

end.
