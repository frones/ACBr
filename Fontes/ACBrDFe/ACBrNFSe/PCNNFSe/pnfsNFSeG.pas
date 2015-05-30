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

unit pnfsNFSeG;

interface

uses
  SysUtils, Classes, Forms, pcnAuxiliar,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pnfsNFSe, pnfsConversao, ACBrUtil, ACBrDFeUtil, StrUtils, DateUtils;

type

  TNFSeG = class
   private
     class function GetIdEntidadeEquiplano(const IBGE: Integer): String;
   protected

   public
     class function Gera_DadosMsgEnviarLote(Prefixo3, Prefixo4, Identificador,
                                    NameSpaceDad, VersaoDados, VersaoXML,
                                    NumeroLote, CNPJ, IM, QtdeNotas: String;
                                    Notas, TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum): AnsiString;

     class function Gera_DadosMsgConsSitLote(Prefixo3, Prefixo4, NameSpaceDad,
                                     VersaoXML, Protocolo, CNPJ, IM: String;
                                     TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum): AnsiString;

     class function Gera_DadosMsgConsLote(Prefixo3, Prefixo4, NameSpaceDad,
                                  VersaoXML, Protocolo, CNPJ, IM, senha, frase_secreta: String;
                                  TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum; ARazaoSocial: String = ''): AnsiString;

     class function Gera_DadosMsgConsNFSeRPS(Prefixo3, Prefixo4, NameSpaceDad, VersaoXML,
                                     NumeroRps, SerieRps, TipoRps, CNPJ, IM, senha, frase_secreta: String;
                                     TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum; ARazaoSocial: String = ''): AnsiString;

     class function Gera_DadosMsgConsNFSe(Prefixo3, Prefixo4, NameSpaceDad, VersaoXML,
                                          CNPJ, IM: String;
                                          DataInicial, DataFinal: TDateTime;
                                          TagI, TagF: AnsiString; NumeroNFSe: string = '';
                                          Senha : string = ''; FraseSecreta : string = '';
                                          AProvedor: TnfseProvedor = proNenhum;
                                          APagina: Integer = 1; CNPJTomador: String = ''; IMTomador: String = '';
                                          NomeInter: String = ''; CNPJInter: String = ''; IMInter: String = ''): AnsiString;

     // Alterado por Augusto Fontana - 28/04/2014. Inclusão do parametromo motivo do cancelamento
     class function Gera_DadosMsgCancelarNFSe(Prefixo4, NameSpaceDad, NumeroNFSe, CNPJ, IM,
                                      CodMunicipio, CodCancelamento: String;
                                      TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum;
                                      MotivoCancelamento: String = ''): AnsiString;

     class function Gera_DadosMsgGerarNFSe(Prefixo3, Prefixo4, Identificador,
                                   NameSpaceDad, VersaoDados, VersaoXML,
                                   NumeroLote, CNPJ, IM, QtdeNotas: String;
                                   Notas, TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum): AnsiString;

     class function Gera_DadosMsgEnviarSincrono(Prefixo3, Prefixo4, Identificador,
                                        NameSpaceDad, VersaoDados, VersaoXML,
                                        NumeroLote, CNPJ, IM, QtdeNotas: String;
                                        Notas, TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum): AnsiString;

     class function Gera_DadosMsgSubstituirNFSe(Prefixo3, Prefixo4, Identificador,
                                   NameSpaceDad, NumeroNFSe, CNPJ, IM,
                                   CodMunicipio, CodCancelamento,
                                   MotivoCancelamento, VersaoDados, VersaoXML,
                                   NumeroLote, QtdeNotas: String;
                                   Notas, TagI, TagF: AnsiString;
                                   AProvedor: TnfseProvedor = proNenhum): AnsiString;

     //-------------------------------------------------------------------------
     // As classes abaixo são exclusivas para o provedor DSF
     //-------------------------------------------------------------------------
     class function Gera_DadosMsgEnviarLoteDSF(Prefixo3, Prefixo4,Identificador, NameSpaceDad, VersaoXML,
                                               NumeroLote, CodCidade, CNPJ, IM, RazaoSocial, Transacao,
                                               QtdeNotas, ValorTotalServicos, ValorTotalDeducoes: String;
                                               DataInicial, DataFinal: TDateTime;
                                               Notas, TagI, TagF: AnsiString): AnsiString;

     class function Gera_DadosMsgConsLoteDSF(Prefixo3, Prefixo4, NameSpaceDad,
                                             VersaoXML, CodCidade, CNPJ, NumeroLote: String;
                                             TagI, TagF: AnsiString): AnsiString;

     class function Gera_DadosMsgConsNFSeRPSDSF(Prefixo3, Prefixo4, NameSpaceDad,VersaoXML,
                                                CodCidade, CNPJ, Transacao, NumeroLote: String;
                                                Notas, TagI, TagF: AnsiString): AnsiString;

     class function Gera_DadosMsgConsNFSeDSF(Prefixo3, Prefixo4, NameSpaceDad, VersaoXML, CodCidade,
                                             CNPJ, IM, NotaInicial: String; DataInicial, DataFinal: TDateTime;
                                             TagI, TagF: AnsiString): AnsiString;

     class function Gera_DadosMsgCancelarNFSeDSF(Prefixo3, Prefixo4, NameSpaceDad, VersaoXML,
                                                 CNPJ, Transacao, CodMunicipio, NumeroLote: String;
                                                 Notas, TagI, TagF: AnsiString): AnsiString;

     class function Gera_DadosMsgConsSeqRPSDSF(TagI, TagF: AnsiString; VersaoXML, CodCidade,
                                               IM, CNPJ, SeriePrestacao: String): AnsiString;

     //-------------------------------------------------------------------------
     // As classes abaixo são exclusivas para o provedor Infisc
     //-------------------------------------------------------------------------
     class function Gera_DadosMsgEnviarLoteInfisc(Prefixo3, Prefixo4,Identificador, NameSpaceDad, VersaoXML,
                                               NumeroLote, CodCidade, CNPJ, IM, RazaoSocial, Transacao,
                                               QtdeNotas, ValorTotalServicos, ValorTotalDeducoes: String;
                                               DataInicial, DataFinal: TDateTime;
                                               Notas, TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsSitLoteInfisc(CodCidade: Integer;
                                                   CNPJ, IM, Protocolo, NumeroLote: String;
                                                   TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsNFSeInfisc(Prefixo3, Prefixo4, NameSpaceDad, VersaoXML, CodCidade,
                                                CNPJ, IM, NotaInicial, Serie: String; DataInicial, DataFinal: TDateTime;
                                                TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgCancelarNFSeInfisc(Prefixo3, Prefixo4, NameSpaceDad, VersaoXML,
                                                    CNPJ, Transacao, CodMunicipio, NumeroLote: String;
                                                    Notas, TagI, TagF: AnsiString): AnsiString;

     //As classes abaixos são exclusivas do provedor Equiplano
     class function Gera_DadosMsgEnviarLoteEquiplano(VersaoXML, NumeroLote, QtdeRPS, CNPJ, IM: String;
                                                     CodCidade: Integer;
                                                     OptanteSimples: Boolean;
                                                     Notas, TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsLoteEquiplano(CodCidade: Integer;
                                                   CNPJ, IM, Protocolo, NumeroLote: String;
                                                   TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsNFSeRPSEquiplano(CodCidade: Integer;
                                                      NumeroRps, CNPJ, IM: String;
                                                      TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgCancelarNFSeEquiplano(CodCidade: Integer;
                                                       CNPJ, IM, NumeroNFSe, MotivoCancelamento: String;
                                                       TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsSitLoteEquiplano(CodCidade: Integer;
                                                      CNPJ, IM, Protocolo, NumeroLote: String;
                                                      TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgCancelarNFSeFreire(Prefixo4, NameSpaceDad, NumeroNFSe, CNPJ, IM,
                                                    CodMunicipio, CodCancelamento, MotivoCancelamento: String;
                                                    TagI, TagF: AnsiString): AnsiString;

     //As classes abaixos são exclusivas do provedor EL
     class function Gera_DadosMsgEnviarLoteEL(NameSpaceDad, NumeroLote,
       QtdeRPS, CNPJ, IM: String; CodCidade: Integer;
       OptanteSimples: Boolean; Id, Notas, TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsSitLoteEL(CodCidade: Integer; CNPJ, IM,
       Protocolo, NumeroLote: String; TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsLoteEL(CodCidade: Integer; CNPJ, IM,
       Protocolo, NumeroLote: String; TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsNFSeRPSEL(CodCidade: Integer;
       NumeroRps, CNPJ, IM: String; TagI, TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsNFSeEL(Prefixo3, Prefixo4,
       NameSpaceDad, VersaoXML, NumeroNFSe, CNPJ, IM, CNPJTomador,
       CNPJITermediarioServ: String; DataInicial, DataFinal: TDateTime; TagI,
       TagF: AnsiString): AnsiString;
     class function Gera_DadosMsgConsSeqRPSEL(TagI, TagF: AnsiString;
       VersaoXML, CodCidade, IM, CNPJ, SeriePrestacao: String): AnsiString;
     class function Gera_DadosMsgCancelarNFSeEL(CodCidade: Integer; CNPJ,
       IM, NumeroNFSe, MotivoCancelamento: String; TagI,
       TagF: AnsiString): AnsiString;
   published

   end;

implementation

//uses
// IniFiles, Variants, ACBrConsts;

class function TNFSeG.Gera_DadosMsgEnviarLote(Prefixo3, Prefixo4,
  Identificador, NameSpaceDad, VersaoDados, VersaoXML, NumeroLote, CNPJ,
  IM, QtdeNotas: String; Notas, TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum): AnsiString;
var
 DadosMsg: AnsiString;
 IdLote,tagCabecalhoCodigoMunicipio: String;
begin
 if AProvedor = proBetha then Prefixo3 := '';
 if AProvedor in [proGovBR, proPronim, proISSDigital] then Identificador := '';



 case AProvedor of
  proTecnos : IdLote := '1' + IntToStrZero(YearOf(Date), 4) +
                        Copy(Notas, Pos('<InfDeclaracaoPrestacaoServico Id="', Notas) + 40, 14) +
                        IntToStrZero(StrToIntDef(NumeroLote, 1), 16);
  else IdLote := NumeroLote;
 end;

 DadosMsg := '<' + Prefixo3 + 'LoteRps' +

               // Inclui a versão antes do Id para proCoplan
               ifThen(AProvedor in [proCoplan],
                               ifThen(VersaoDados <> '', ' versao="' + VersaoDados + '"', '' ),
                               '') +

               // Inclui o Identificador ou não
               ifThen(Identificador <> '', ' ' + Identificador + '="' + IdLote + '"', '') +

               // Não Incluir a versão para os provedores abaixo
               ifThen(AProvedor in [proAbaco, proBetha, proDBSeller,
                                             proGinfes, proGoiania, proGovBR, proIssCuritiba,
                                             proISSNET, proLexsom, proNatal, proTinus, proRecife, proRJ,
                                             proSimplISS, proThema, proTiplan, proAgili,
                                             proFISSLex, proSpeedGov, proPronim, proCoplan,
                                             proSalvador, proSJP, proFintelISS],
                               '',
                               ifThen(VersaoDados <> '', ' versao="' + VersaoDados + '"', '')
                              ) +

               // Inclui a versão com V em maiusculo
               ifThen(AProvedor in [proFintelISS],
                               ifThen(VersaoDados <> '', ' Versao="' + VersaoDados + '"', '' ),
                               '') +

               // Inclui o Name Space ou não
               ifThen(AProvedor = proSimplISS,
                               ' ' + NameSpaceDad,
                               '>'
                              ) +
                              
              '<' + Prefixo4 + 'NumeroLote>' +
                NumeroLote +
              '</' + Prefixo4 + 'NumeroLote>' +

              ifThen((VersaoXML = '2') or
                              (AProvedor in [proISSNet, proActcon]),

                '<' + Prefixo4 + 'CpfCnpj>' +
                ifThen(Length(OnlyNumber(Cnpj)) <= 11,
                 '<' + Prefixo4 + 'Cpf>' +
                   Cnpj +
                 '</' + Prefixo4 + 'Cpf>',
                 '<' + Prefixo4 + 'Cnpj>' +
                   Cnpj +
                 '</' + Prefixo4 + 'Cnpj>') +
                '</' + Prefixo4 + 'CpfCnpj>',

                '<' + Prefixo4 + 'Cnpj>' +
                  Cnpj +
                '</' + Prefixo4 + 'Cnpj>') +

              '<' + Prefixo4 + 'InscricaoMunicipal>' +
                IM +
              '</' + Prefixo4 + 'InscricaoMunicipal>' +
              '<' + Prefixo4 + 'QuantidadeRps>' +
                QtdeNotas +
              '</' + Prefixo4 + 'QuantidadeRps>' +
              '<' + Prefixo4 + 'ListaRps>' +
               Notas +
              '</' + Prefixo4 + 'ListaRps>' +
             '</' + Prefixo3 + 'LoteRps>';

 Result := TagI + DadosMsg + TagF;
 

 tagCabecalhoCodigoMunicipio := RetornarConteudoEntre(Notas,'<CodigoMunicipio>','</CodigoMunicipio>');
 tagCabecalhoCodigoMunicipio := ' codMunicipio="'+tagCabecalhoCodigoMunicipio+'"';

  // Luiz Baião 2014.11.24 -
 if  AProvedor = proNFSEBrasil then begin
                   DadosMsg := '<' + Prefixo3 + 'LoteRps'+ tagCabecalhoCodigoMunicipio +
//                       ifThen(codMunicipio <> '', ' codMunicipio="' + codMunicipio + '"','') +
                       ifThen(VersaoDados <> '', ' versao="' + VersaoDados + '"','') +
                       ifThen(Identificador <> '', ' ' + Identificador + '="' + NumeroLote + '"', '') +

                        '>' +
                      '<' + Prefixo4 + 'NumeroLote>' +
                        NumeroLote +
                      '</' + Prefixo4 + 'NumeroLote>' +

                      ifThen(VersaoXML = '1',

                        '<' + Prefixo4 + 'CpfCnpj>' +
                        '<' + Prefixo4 + 'Cnpj>' +
                          Cnpj +
                        '</' + Prefixo4 + 'Cnpj>' +
                        '</' + Prefixo4 + 'CpfCnpj>',

                        '<' + Prefixo4 + 'Cnpj>' +
                          Cnpj +
                        '</' + Prefixo4 + 'Cnpj>') +

                      '<' + Prefixo4 + 'InscricaoMunicipal>' +
                        IM +
                      '</' + Prefixo4 + 'InscricaoMunicipal>' +
                      '<' + Prefixo4 + 'QuantidadeRps>' +
                        QtdeNotas +
                      '</' + Prefixo4 + 'QuantidadeRps>' +
                      '<' + Prefixo4 + 'ListaRps>' +
                       Notas +
                      '</' + Prefixo4 + 'ListaRps>' +
                     '</' + Prefixo3 + 'LoteRps>';

         Result := TagI + DadosMsg + TagF;
 end;		 

 if AProvedor in [proNenhum, proABRASFv1, proABRASFv2] then Result := '';
end;

class function TNFSeG.Gera_DadosMsgConsSitLote(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, Protocolo, CNPJ, IM: String; TagI,
  TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum): AnsiString;
var
 DadosMsg: AnsiString;
begin
 if AProvedor = proBetha then Prefixo3 := '';

 DadosMsg := '<' + Prefixo3 + 'Prestador' +
               ifThen(AProvedor = proSimplISS, ' ' + NameSpaceDad, '>') +
               ifThen((VersaoXML = '2') or
                               (AProvedor in [proISSNet, proActcon]),

                 '<' + Prefixo4 + 'CpfCnpj>' +
                  ifThen(Length(OnlyNumber(Cnpj)) <= 11,
                  '<' + Prefixo4 + 'Cpf>' +
                    Cnpj +
                  '</' + Prefixo4 + 'Cpf>',
                  '<' + Prefixo4 + 'Cnpj>' +
                    Cnpj +
                  '</' + Prefixo4 + 'Cnpj>') +
                 '</' + Prefixo4 + 'CpfCnpj>',

                 '<' + Prefixo4 + 'Cnpj>' +
                   Cnpj +
                 '</' + Prefixo4 + 'Cnpj>') +

               '<' + Prefixo4 + 'InscricaoMunicipal>' +
                 IM +
               '</' + Prefixo4 + 'InscricaoMunicipal>' +
              '</' + Prefixo3 + 'Prestador>' +
              '<' + Prefixo3 + 'Protocolo' +
               ifThen(AProvedor = proSimplISS, ' ' + NameSpaceDad, '>') +
                Protocolo +
              '</' + Prefixo3 + 'Protocolo>';

 if AProvedor = proDBSeller then
   DadosMsg := '<ConsultarSituacaoLoteRpsEnvio>' + DadosMsg + '</ConsultarSituacaoLoteRpsEnvio>';

 Result := TagI + DadosMsg + TagF;

 if AProvedor in [proNenhum, proABRASFv1, proABRASFv2, pro4R, proAgili,
                  proCoplan, profintelISS, proFiorilli, proGoiania, proGovDigital,
                  proISSDigital, proISSe, proProdata, proVirtual, proSaatri,
                  proFreire, proPVH, proVitoria, proTecnos, proSisPMJP,
                  proSystemPro] then Result := '';
end;

class function TNFSeG.Gera_DadosMsgConsLote(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, Protocolo, CNPJ, IM, senha, frase_secreta: String; TagI,
  TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum; ARazaoSocial: String = ''): AnsiString;
var
 DadosMsg: AnsiString;
begin
 if AProvedor = proBetha then Prefixo3 := '';

 DadosMsg := '<' + Prefixo3 + 'Prestador' +
               ifThen(AProvedor = proSimplISS, ' ' + NameSpaceDad, '>') +

               ifThen((VersaoXML = '2') or
                               (AProvedor in [proISSNet, proActcon]),

                 '<' + Prefixo4 + 'CpfCnpj>' +
                  ifThen(Length(OnlyNumber(Cnpj)) <= 11,
                  '<' + Prefixo4 + 'Cpf>' +
                    Cnpj +
                  '</' + Prefixo4 + 'Cpf>',
                  '<' + Prefixo4 + 'Cnpj>' +
                    Cnpj +
                  '</' + Prefixo4 + 'Cnpj>') +
                 '</' + Prefixo4 + 'CpfCnpj>',

                 '<' + Prefixo4 + 'Cnpj>' +
                   Cnpj +
                 '</' + Prefixo4 + 'Cnpj>') +
               ifThen(AProvedor = proTecnos, '<RazaoSocial>' + ARazaoSocial + '</RazaoSocial>' , '') +
               '<' + Prefixo4 + 'InscricaoMunicipal>' +
                 IM +
               '</' + Prefixo4 + 'InscricaoMunicipal>' +

              ifThen(AProvedor = proISSDigital,
               '<' + Prefixo4 + 'Senha>' +
                 Senha +
               '</' + Prefixo4 + 'Senha>' +
               '<' + Prefixo4 + 'FraseSecreta>' +
                 frase_secreta +
               '</' + Prefixo4 + 'FraseSecreta>', '') +

              '</' + Prefixo3 + 'Prestador>' +
              '<' + Prefixo3 + 'Protocolo' +
               ifThen(AProvedor = proSimplISS, ' ' + NameSpaceDad, '>') +
                Protocolo +
              '</' + Prefixo3 + 'Protocolo>';

 Result := TagI + DadosMsg + TagF;

 if AProvedor in [proNenhum, proABRASFv1, proABRASFv2] then Result := '';
end;

class function TNFSeG.Gera_DadosMsgConsNFSeRPS(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, NumeroRps, SerieRps, TipoRps, CNPJ, IM,
  senha, frase_secreta: String;
  TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum; ARazaoSocial: String = ''): AnsiString;
var
 DadosMsg: AnsiString;
begin
 if AProvedor = proBetha then Prefixo3 := '';

 DadosMsg := '<' + Prefixo3 + 'IdentificacaoRps' +
               ifThen(AProvedor = proSimplISS, ' ' + NameSpaceDad, '>') +
              '<' + Prefixo4 + 'Numero>' +
                NumeroRps +
              '</' + Prefixo4 + 'Numero>' +
              '<' + Prefixo4 + 'Serie>' +
                SerieRps +
              '</' + Prefixo4 + 'Serie>' +
              '<' + Prefixo4 + 'Tipo>' +
                TipoRps +
              '</' + Prefixo4 + 'Tipo>' +
             '</' + Prefixo3 + 'IdentificacaoRps>' +
             '<' + Prefixo3 + 'Prestador' +
               ifThen(AProvedor = proSimplISS, ' ' + NameSpaceDad, '>') +

              ifThen((VersaoXML = '2') or
                              (AProvedor in [proISSNet, proActcon]),

                '<' + Prefixo4 + 'CpfCnpj>' +
                  ifThen(Length(OnlyNumber(Cnpj)) <= 11,
                  '<' + Prefixo4 + 'Cpf>' +
                    Cnpj +
                  '</' + Prefixo4 + 'Cpf>',
                  '<' + Prefixo4 + 'Cnpj>' +
                    Cnpj +
                  '</' + Prefixo4 + 'Cnpj>') +
                '</' + Prefixo4 + 'CpfCnpj>',

                '<' + Prefixo4 + 'Cnpj>' +
                  Cnpj +
                '</' + Prefixo4 + 'Cnpj>') +
              ifThen(AProvedor = proTecnos, '<RazaoSocial>' + ARazaoSocial + '</RazaoSocial>', '') +
              '<' + Prefixo4 + 'InscricaoMunicipal>' +
                IM +
              '</' + Prefixo4 + 'InscricaoMunicipal>' +

              ifThen(AProvedor = proISSDigital,
               '<' + Prefixo4 + 'Senha>' +
                 Senha +
               '</' + Prefixo4 + 'Senha>' +
               '<' + Prefixo4 + 'FraseSecreta>' +
                 frase_secreta +
               '</' + Prefixo4 + 'FraseSecreta>', '') +

             '</' + Prefixo3 + 'Prestador>';

 if AProvedor = proDBSeller then
   DadosMsg := '<ConsultarNfseRpsEnvio>' + DadosMsg + '</ConsultarNfseRpsEnvio>';

 Result := TagI + DadosMsg + TagF;

 if AProvedor in [proNenhum, proABRASFv1, proABRASFv2] then Result := '';
end;

class function TNFSeG.Gera_DadosMsgConsNFSe(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, CNPJ, IM: String; DataInicial, DataFinal: TDateTime; TagI,
  TagF: AnsiString; NumeroNFSe: string = ''; Senha : string = '';
  FraseSecreta : string = ''; AProvedor: TnfseProvedor = proNenhum;
  APagina: Integer = 1; CNPJTomador: String = ''; IMTomador: String = '';
  NomeInter: String = ''; CNPJInter: String = ''; IMInter: String = ''): AnsiString;
var
 DadosMsg: AnsiString;
begin
 if AProvedor = proBetha then Prefixo3 := '';

 DadosMsg := '<' + Prefixo3 + 'Prestador' +
               ifThen(AProvedor = proSimplISS, ' ' + NameSpaceDad, '>') +
               ifThen((VersaoXML = '2') or
                               (AProvedor in [proISSNet, proActcon]),

                 '<' + Prefixo4 + 'CpfCnpj>' +
                  ifThen(Length(OnlyNumber(Cnpj)) <= 11,
                  '<' + Prefixo4 + 'Cpf>' +
                    Cnpj +
                  '</' + Prefixo4 + 'Cpf>',
                  '<' + Prefixo4 + 'Cnpj>' +
                    Cnpj +
                  '</' + Prefixo4 + 'Cnpj>') +
                 '</' + Prefixo4 + 'CpfCnpj>',

                 '<' + Prefixo4 + 'Cnpj>' +
                  Cnpj +
                 '</' + Prefixo4 + 'Cnpj>') +

               '<' + Prefixo4 + 'InscricaoMunicipal>' +
                IM +
               '</' + Prefixo4 + 'InscricaoMunicipal>' +

              ifThen(AProvedor = proISSDigital,
               '<' + Prefixo4 + 'Senha>' +
                 Senha +
               '</' + Prefixo4 + 'Senha>' +
               '<' + Prefixo4 + 'FraseSecreta>' +
                 FraseSecreta +
               '</' + Prefixo4 + 'FraseSecreta>', '') +

              '</' + Prefixo3 + 'Prestador>';

 if NumeroNFSe <> '' then
 begin
  if AProvedor in [proPVH, proSystemPro, proPublica, proSisPMJP] then
    DadosMsg := DadosMsg + '<Faixa>' +
                             '<NumeroNfseInicial>' + NumeroNFSe + '</NumeroNfseInicial>' +
                             '<NumeroNfseFinal>' + NumeroNFSe + '</NumeroNfseFinal>' +
                           '</Faixa>'
  else
    DadosMsg := DadosMsg + '<' + Prefixo3 + 'NumeroNfse>' +
                             NumeroNFSe +
                           '</' + Prefixo3 + 'NumeroNfse>';
 end;

 if (DataInicial>0) and (DataFinal>0)
  then DadosMsg := DadosMsg + '<' + Prefixo3 + 'PeriodoEmissao>' +
                               '<' + Prefixo3 + 'DataInicial>' +
                                 FormatDateTime('yyyy-mm-dd', DataInicial) +
                               '</' + Prefixo3 + 'DataInicial>' +
                               '<' + Prefixo3 + 'DataFinal>' +
                                 FormatDateTime('yyyy-mm-dd', DataFinal) +
                               '</' + Prefixo3 + 'DataFinal>' +
                              '</' + Prefixo3 + 'PeriodoEmissao>';

 if (CNPJTomador <> '') or (IMTomador <> '')
  then begin
    DadosMsg := DadosMsg + '<Tomador>' +
                            '<' + Prefixo4 + 'CpfCnpj>' +
                             ifThen(Length(OnlyNumber(CnpjTomador)) <= 11,
                             '<' + Prefixo4 + 'Cpf>' +
                               CnpjTomador +
                             '</' + Prefixo4 + 'Cpf>',
                             '<' + Prefixo4 + 'Cnpj>' +
                               CnpjTomador +
                             '</' + Prefixo4 + 'Cnpj>') +
                            '</' + Prefixo4 + 'CpfCnpj>' +
                            '<' + Prefixo4 + 'InscricaoMunicipal>' +
                             IMTomador +
                            '</' + Prefixo4 + 'InscricaoMunicipal>' +
                           '</Tomador>'
(*
    DadosMsg := DadosMsg + '<Tomador>' +
                            ifThen((VersaoXML = '2') or
                                            (AProvedor in [proThema]),

                            '<' + Prefixo4 + 'CpfCnpj>' +
                             ifThen(Length(OnlyNumber(CnpjTomador)) <= 11,
                             '<' + Prefixo4 + 'Cpf>' +
                               CnpjTomador +
                             '</' + Prefixo4 + 'Cpf>',
                             '<' + Prefixo4 + 'Cnpj>' +
                               CnpjTomador +
                             '</' + Prefixo4 + 'Cnpj>') +
                            '</' + Prefixo4 + 'CpfCnpj>',

                            '<' + Prefixo4 + 'Cnpj>' +
                             CnpjTomador +
                            '</' + Prefixo4 + 'Cnpj>') +

                            '<' + Prefixo4 + 'InscricaoMunicipal>' +
                             IMTomador +
                            '</' + Prefixo4 + 'InscricaoMunicipal>' +
                           '</Tomador>'
*)
  end;

 if (NomeInter <> '') and (CNPJInter <> '')
  then begin
    DadosMsg := DadosMsg + '<IntermediarioServico>' +
                            '<' + Prefixo4 + 'RazaoSocial>' +
                             NomeInter +
                            '</' + Prefixo4 + 'RazaoSocial>' +

                            ifThen(VersaoXML = '2',

                            '<' + Prefixo4 + 'CpfCnpj>' +
                             ifThen(Length(OnlyNumber(CnpjInter)) <= 11,
                             '<' + Prefixo4 + 'Cpf>' +
                               CnpjInter +
                             '</' + Prefixo4 + 'Cpf>',
                             '<' + Prefixo4 + 'Cnpj>' +
                               CnpjInter +
                             '</' + Prefixo4 + 'Cnpj>') +
                            '</' + Prefixo4 + 'CpfCnpj>',

                            '<' + Prefixo4 + 'Cnpj>' +
                             CnpjInter +
                            '</' + Prefixo4 + 'Cnpj>') +

                            '<' + Prefixo4 + 'InscricaoMunicipal>' +
                             IMInter +
                            '</' + Prefixo4 + 'InscricaoMunicipal>' +
                           '</IntermediarioServico>'
  end;

 if AProvedor in [proFiorilli, profintelISS, proPVH, proSystemPro, proSisPMJP]
  then DadosMsg := DadosMsg + '<' + Prefixo3 + 'Pagina>' +
                                IntToStr(APagina) +
                              '</' + Prefixo3 + 'Pagina>';

 Result := TagI + DadosMsg + TagF;

 if AProvedor in [proNenhum, proABRASFv1, proABRASFv2] then Result := '';
end;

class function TNFSeG.Gera_DadosMsgCancelarNFSe(Prefixo4, NameSpaceDad, NumeroNFSe,
  CNPJ, IM, CodMunicipio, CodCancelamento: String; TagI,
  TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum; MotivoCancelamento: String = ''): AnsiString;
var
 DadosMsg: AnsiString;
begin
 case AProvedor of
  proGinfes: DadosMsg := '<Prestador>' +
                          '<' + Prefixo4 + 'Cnpj>' +
                            Cnpj +
                          '</' + Prefixo4 + 'Cnpj>' +
                          '<' + Prefixo4 + 'InscricaoMunicipal>' +
                            IM +
                          '</' + Prefixo4 + 'InscricaoMunicipal>' +
                         '</Prestador>' +
                         '<NumeroNfse>' +
                           NumeroNFSe +
                         '</NumeroNfse>';


  // Adicionado por Akai - L. Massao Aihara 31/10/2013
  proIssCuritiba: DadosMsg := '<InfPedidoCancelamento>' +
                               '<IdentificacaoNfse>' +
                                '<Cnpj>' + Cnpj + '</Cnpj>' +
                                '<InscricaoMunicipal>' + IM + '</InscricaoMunicipal>' +
                                '<Numero>' + NumeroNFse + '</Numero>' +
                               '</IdentificacaoNfse>' +
                              '</InfPedidoCancelamento>';


  else
    begin
      DadosMsg :=  '<' + Prefixo4 + 'IdentificacaoNfse>' +
                    '<' + Prefixo4 + 'Numero>' +
                      NumeroNFse +
                    '</' + Prefixo4 + 'Numero>' +

                    // alterado por Akai - L. Massao Aihara 12/11/2013
                   ifThen(AProvedor in [pro4R, proISSe, profintelISS, proFiorilli,
                                                 proDigifred, proSystempro, proVirtual,
                                                 proISSDigital, proSaatri, proCoplan,
                                                 proVitoria, proTecnos, proPVH,
                                                 proSisPMJP, proActcon, proGovDigital],

                    //Adicionei o IfThen para poder cancelar nota onde o pretador é pessoa física (Cartório em Vitória-ES). - Eduardo Silva dos Santos - 11/01/2014 - DRD SISTEMAS
                    ifThen( length(Cnpj)=14,
                                                 ('<' + Prefixo4 + 'CpfCnpj>' +
                                                   '<' + Prefixo4 + 'Cnpj>' +
                                                    Cnpj +
                                                   '</' + Prefixo4 + 'Cnpj>' +
                                                  '</' + Prefixo4 + 'CpfCnpj>')
                                                    ,
                                                 ('<' + Prefixo4 + 'CpfCnpj>' +
                                                   '<' + Prefixo4 + 'Cpf>' +
                                                    Cnpj +
                                                   '</' + Prefixo4 + 'Cpf>' +
                                                  '</' + Prefixo4 + 'CpfCnpj>')
                                   ),

                    '<' + Prefixo4 + 'Cnpj>' +
                      Cnpj +
                    '</' + Prefixo4 + 'Cnpj>') +

                    '<' + Prefixo4 + 'InscricaoMunicipal>' +
                      IM +
                    '</' + Prefixo4 + 'InscricaoMunicipal>' +
                    '<' + Prefixo4 + 'CodigoMunicipio>' +
                      CodMunicipio +
                    '</' + Prefixo4 + 'CodigoMunicipio>' +
                   '</' + Prefixo4 + 'IdentificacaoNfse>' +
                   '<' + Prefixo4 + 'CodigoCancelamento>' +

                     // Codigo de Cancelamento
                     // 1 - Erro de emissão
                     // 2 - Serviço não concluido
                     // 3 - RPS Cancelado na Emissão

                     CodCancelamento +

                   '</' + Prefixo4 + 'CodigoCancelamento>';
                  // Alterado por Augusto Fontana - 28/04/2014. Incluir do motivo do cancelamento
                  if (AProvedor in [proPublica, proTecnos]) and (MotivoCancelamento <> '') then
                    begin
                      DadosMsg := DadosMsg + '<' + Prefixo4 + 'MotivoCancelamento>';
                      DadosMsg := DadosMsg + MotivoCancelamento;
                      DadosMsg := DadosMsg + '</' + Prefixo4 + 'MotivoCancelamento>';
                    end;
                  DadosMsg := DadosMsg + '</' + Prefixo4 + 'InfPedidoCancelamento>';
    end;
 end;

 Result := TagI + DadosMsg + TagF;

 if AProvedor = proDBSeller then
   Result := '<CancelarNfse>' + Result + '</CancelarNfse>';

 if AProvedor in [proNenhum, proABRASFv1, proABRASFv2] then Result := '';
end;

class function TNFSeG.Gera_DadosMsgGerarNFSe(Prefixo3, Prefixo4,
  Identificador, NameSpaceDad, VersaoDados, VersaoXML, NumeroLote, CNPJ,
  IM, QtdeNotas: String; Notas, TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum): AnsiString;
var
 DadosMsg: AnsiString;
begin
 if AProvedor = proBetha then Prefixo3 := '';

 case AProvedor of
  proWebISS: begin
               DadosMsg := '<' + Prefixo3 + 'LoteRps'+
                              ifThen(Identificador <> '', ' ' + Identificador + '="' + NumeroLote + '"', '') +
                              ifThen(VersaoDados <> '', ' versao="' + VersaoDados + '"', '') + '>' +
                             '<' + Prefixo4 + 'NumeroLote>' +
                               NumeroLote +
                             '</' + Prefixo4 + 'NumeroLote>' +

                             ifThen((VersaoXML = '2') or (AProvedor = proISSNet),

                             '<' + Prefixo4 + 'CpfCnpj>' +
                                ifThen(Length(OnlyNumber(Cnpj)) <= 11,
                               '<' + Prefixo4 + 'Cpf>' +
                                  Cnpj +
                               '</' + Prefixo4 + 'Cpf>',
                               '<' + Prefixo4 + 'Cnpj>' +
                                  Cnpj +
                               '</' + Prefixo4 + 'Cnpj>') +
                             '</' + Prefixo4 + 'CpfCnpj>',

                             '<' + Prefixo4 + 'Cnpj>' +
                                Cnpj +
                             '</' + Prefixo4 + 'Cnpj>') +

                             '<' + Prefixo4 + 'InscricaoMunicipal>' +
                                IM +
                             '</' + Prefixo4 + 'InscricaoMunicipal>' +
                             '<' + Prefixo4 + 'QuantidadeRps>' +
                                QtdeNotas +
                             '</' + Prefixo4 + 'QuantidadeRps>' +
                             '<' + Prefixo4 + 'ListaRps>' +
                                Notas +
                             '</' + Prefixo4 + 'ListaRps>' +
                           '</' + Prefixo3 + 'LoteRps>';

               Result := TagI + DadosMsg + TagF;
             end;
  else Result := TagI + Notas + TagF;
 end;

 if AProvedor in [proNenhum, proABRASFv1, proABRASFv2, proAbaco,
                  proBetha, proBetim, proBHIss, proDBSeller, {proDigifred,}
                  proEquiplano, proFIssLex, proGinfes, proGovBR, proIssCuritiba,
                  proIssIntel, proIssNet, proLexsom, proNatal, proTinus, proProdemge,
                  proRJ, proSimplIss, proThema, proTiplan, proIssDSF, proInfisc, proAgili,
                  proSpeedGov, proPronim, proActcon,
                  proSalvador, proNFSEBrasil] then Result := '';
end;

class function TNFSeG.Gera_DadosMsgEnviarSincrono(Prefixo3, Prefixo4,
  Identificador, NameSpaceDad, VersaoDados, VersaoXML, NumeroLote, CNPJ,
  IM, QtdeNotas: String; Notas, TagI, TagF: AnsiString; AProvedor: TnfseProvedor = proNenhum): AnsiString;
begin
 Result := Gera_DadosMsgEnviarLote(Prefixo3, Prefixo4, Identificador, NameSpaceDad,
                                   VersaoDados, VersaoXML, NumeroLote, CNPJ, IM,
                                   QtdeNotas, Notas, TagI, TagF, AProvedor);

 if AProvedor in [proNenhum, proABRASFv1, proABRASFv2, proAbaco,
                  proBetha, proBetim, proBHISS, proDBSeller, proDigifred,
                  proEquiplano, profintelISS, proFISSLex, proGinfes, proGoiania,
                  proGovBR, {proGovDigital,} proIssCuritiba, proISSDigital,
                  proISSIntel, proISSNet, proLexsom, proNatal, proTinus, proProdemge,
                  proPublica, proRecife, proRJ, proSaatri, proFreire,
                  proSimplISS, proThema, proTiplan, proWebISS, proProdata,
                  proAgili, proSpeedGov, proPronim,
                  proSalvador,proNFSEBrasil] then Result := '';
end;

class function TNFSeG.Gera_DadosMsgSubstituirNFSe(Prefixo3, Prefixo4, Identificador,
                                   NameSpaceDad, NumeroNFSe, CNPJ, IM,
                                   CodMunicipio, CodCancelamento,
                                   MotivoCancelamento, VersaoDados,
                                   VersaoXML, NumeroLote, QtdeNotas: String;
                                   Notas, TagI, TagF: AnsiString;
                                   AProvedor: TnfseProvedor = proNenhum): AnsiString;
var
 DadosMsg: AnsiString;
begin
 DadosMsg := Gera_DadosMsgCancelarNFSe(Prefixo4, NameSpaceDad, NumeroNFSe, CNPJ,
                                     IM, CodMunicipio, CodCancelamento, '', '',
                                     AProvedor, MotivoCancelamento);
 DadosMsg := DadosMsg + '</' + Prefixo3 + 'Pedido>';
 DadosMsg := DadosMsg + Notas;
 
 Result := TagI + DadosMsg + TagF;

 if AProvedor in [proNenhum] then Result := '';
end;

//-------------------------------------------------------------------------
// As classes abaixo são exclusivas para o provedor DSF
//-------------------------------------------------------------------------
class function TNFSeG.Gera_DadosMsgEnviarLoteDSF(Prefixo3, Prefixo4,
  Identificador, NameSpaceDad, VersaoXML, NumeroLote, CodCidade, CNPJ, IM,
  RazaoSocial, Transacao, QtdeNotas, ValorTotalServicos,
  ValorTotalDeducoes: String; DataInicial, DataFinal: TDateTime; Notas,
  TagI, TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin
 DadosMsg := '<Cabecalho>' +
               '<CodCidade>'            + CodCidade   + '</CodCidade>' +
               '<CPFCNPJRemetente>'     + Cnpj        + '</CPFCNPJRemetente>' +
               '<RazaoSocialRemetente>' + RazaoSocial + '</RazaoSocialRemetente>' +
               '<transacao>'            + Transacao   + '</transacao>' +
               '<dtInicio>' + FormatDateTime('yyyy-mm-dd', DataInicial) + '</dtInicio>' +
               '<dtFim>'    + FormatDateTime('yyyy-mm-dd', DataFinal) + '</dtFim>' +
               '<QtdRPS>'               + QtdeNotas               + '</QtdRPS>' +
               '<ValorTotalServicos>'   + StringReplace(ValorTotalServicos, ',', '.', [rfReplaceAll]) + '</ValorTotalServicos>' +
               '<ValorTotalDeducoes>'   + StringReplace(ValorTotalDeducoes, ',', '.', [rfReplaceAll]) + '</ValorTotalDeducoes>' +
               '<Versao>'               + VersaoXML          + '</Versao>' +
               '<MetodoEnvio>'          + 'WS'               + '</MetodoEnvio>' +
             '</Cabecalho>' +
             //'<Lote ' + Identificador + '="Lote:' + NumeroLote + '">' +        //Alterado por Ailton 28/07/2017 Retirado o item "Lote" no provedor DSF da erro
             '<Lote ' + Identificador + '="' + NumeroLote + '">' +
                Notas +
             '</Lote>';

  Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgConsLoteDSF(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, CodCidade, CNPJ, NumeroLote: String; TagI,
  TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin
 DadosMsg := '<Cabecalho>' +
               '<CodCidade>' + CodCidade + '</CodCidade>' +
               '<CPFCNPJRemetente>' + Cnpj + '</CPFCNPJRemetente>' +
               '<Versao>' + VersaoXML + '</Versao>' +
               '<NumeroLote>' + NumeroLote + '</NumeroLote>' +
             '</Cabecalho>';

 Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgConsNFSeRPSDSF(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, CodCidade, CNPJ, Transacao, NumeroLote: String;
  Notas, TagI, TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin
  DadosMsg := '<Cabecalho>' +
               '<CodCidade>' + CodCidade + '</CodCidade>' +
               '<CPFCNPJRemetente>' + Cnpj + '</CPFCNPJRemetente>' +
               '<transacao>' + Transacao + '</transacao>' +
               '<Versao>' + VersaoXML + '</Versao>' +
             '</Cabecalho>' +
             '<Lote  Id="lote:' + NumeroLote + '">' +
                Notas +
             '</Lote>';

 Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgConsNFSeDSF(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, CodCidade, CNPJ, IM, NotaInicial: String;
  DataInicial, DataFinal: TDateTime; TagI, TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin
 DadosMsg := '<Cabecalho Id="Consulta:notas">' +
               '<CodCidade>'         + CodCidade    + '</CodCidade>' +
               '<CPFCNPJRemetente>'  + CNPJ         + '</CPFCNPJRemetente>' +
               '<InscricaoMunicipalPrestador>' + IM + '</InscricaoMunicipalPrestador>' +

               '<dtInicio>' +
                 FormatDateTime('yyyy-mm-dd', DataInicial) +
               '</dtInicio>' +

               '<dtFim>' +
                 FormatDateTime('yyyy-mm-dd', DataFinal) +
               '</dtFim>' +

               '<NotaInicial>' + NotaInicial + '</NotaInicial>' +
               '<Versao>'      + VersaoXML   + '</Versao>' +
             '</Cabecalho>';

 Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgCancelarNFSeDSF(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, CNPJ, Transacao, CodMunicipio,
  NumeroLote: String; Notas, TagI, TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin

 DadosMsg := '<Cabecalho>' +
		         '<CodCidade>'        + CodMunicipio + '</CodCidade>' +
		         '<CPFCNPJRemetente>' + CNPJ      + '</CPFCNPJRemetente> ' +
		         '<transacao>'        + Transacao + '</transacao>' +
		         '<Versao>'           + VersaoXML + '</Versao>' +
	          '</Cabecalho>' +
            // '<Lote Id="Lote:' + NumeroLote + '">' +
            '<Lote  Id="' + NumeroLote + '">' + //Alterado por Ailton 28/07/2017 Retirado o item "Lote" no provedor DSF da erro
                Notas +
             '</Lote>';

 Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgConsSeqRPSDSF(TagI, TagF: AnsiString;
  VersaoXML, CodCidade, IM, CNPJ, SeriePrestacao: String): AnsiString;
var
 DadosMsg: AnsiString;
begin
 //consultar sequencial RPS
 DadosMsg := '<Cabecalho>' +
               '<CodCid>' + CodCidade + '</CodCid>' +
               '<IMPrestador>' + IM + '</IMPrestador>' +
               '<CPFCNPJRemetente>' + CNPJ + '</CPFCNPJRemetente>' +
               '<SeriePrestacao>' + SeriePrestacao + '</SeriePrestacao>' +
               '<Versao>' + VersaoXML + '</Versao>' +
             '</Cabecalho>';

 Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.GetIdEntidadeEquiplano(const IBGE: Integer): String;
begin
  case IBGE of
    4102307: Result:= '23'; // Balsa Nova/PR
    4104501: Result:= '50'; // Capanema/PR
    4104659: Result:= '141';// Carambei/PR
    4107207: Result:= '68'; // Dois Vizinhos/PR
    4108403: Result:= '35'; // Francisco Beltrao/PR
    4109807: Result:= '332';// Ibipora/PR
    4120606: Result:= '28'; // Prudentopolis/PR
    4122008: Result:= '19'; // Rio Azul/PR
    4123501: Result:= '54'; // Santa Helena/PR
    4126306: Result:= '61'; // Senges/PR
    4127106: Result:= '260';// Telemaco Borba/PR
    4127700: Result:= '136';// Toledo/PR
  else
    Result:= '';
  end;
end;

class function TNFSeG.Gera_DadosMsgEnviarLoteEquiplano(VersaoXML, NumeroLote, QtdeRPS, CNPJ,
  IM: String; CodCidade: Integer; OptanteSimples: Boolean; Notas, TagI, TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
             '<lote>' +
               '<nrLote>' + NumeroLote + '</nrLote>' +
               '<qtRps>' + QtdeRPS + '</qtRps>' +
               '<nrVersaoXml>' + VersaoXML + '</nrVersaoXml>' +
               '<prestador>' +
                 '<nrCnpj>' + CNPJ + '</nrCnpj>' +
                 '<nrInscricaoMunicipal>' +  IM + '</nrInscricaoMunicipal>' +
                 '<isOptanteSimplesNacional>' + IfThen(OptanteSimples, '1', '2') + '</isOptanteSimplesNacional>' +
                 '<idEntidade>' + GetIdEntidadeEquiplano(CodCidade) + '</idEntidade>' +
               '</prestador>' +
               '<listaRps>' +
                 Notas +
               '</listaRps>' +
             '</lote>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgConsLoteEquiplano(CodCidade: Integer;
  CNPJ, IM, Protocolo, NumeroLote: String; TagI, TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
             '<prestador>' +
               '<nrInscricaoMunicipal>' + IM + '</nrInscricaoMunicipal>' +
               '<cnpj>' + CNPJ + '</cnpj>' +
               '<idEntidade>' + GetIdEntidadeEquiplano(CodCidade) + '</idEntidade>' +
             '</prestador>' +
             '<nrLoteRps>' + NumeroLote + '</nrLoteRps>' +
             //'<nrProtocolo>' + Protocolo + '</nrProtocolo>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgConsNFSeRPSEquiplano(CodCidade: Integer;
  NumeroRps, CNPJ, IM: String; TagI, TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
             '<rps>' +
               '<nrRps>' + NumeroRps + '</nrRps>' +
               '<nrEmissorRps>1</nrEmissorRps>' +
             '</rps>' +
             '<prestador>' +
               '<nrInscricaoMunicipal>' + IM + '</nrInscricaoMunicipal>' +
               '<cnpj>' + CNPJ + '</cnpj>' +
               '<idEntidade>' + GetIdEntidadeEquiplano(CodCidade) + '</idEntidade>' +
             '</prestador>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(
  CodCidade: Integer; CNPJ, IM, NumeroNFSe, MotivoCancelamento: String;
  TagI, TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
             '<prestador>' +
               '<nrInscricaoMunicipal>' + IM + '</nrInscricaoMunicipal>' +
               '<cnpj>' + CNPJ + '</cnpj>' +
               '<idEntidade>' + GetIdEntidadeEquiplano(CodCidade) + '</idEntidade>' +
             '</prestador>' +
             '<nrNfse>' + NumeroNFSe + '</nrNfse>' +
             '<dsMotivoCancelamento>' + MotivoCancelamento + '</dsMotivoCancelamento>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgCancelarNFSeFreire(Prefixo4, NameSpaceDad, NumeroNFSe, CNPJ, IM, CodMunicipio, CodCancelamento,
  MotivoCancelamento: String; TagI, TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin

   DadosMsg := '<' + Prefixo4 + 'IdentificacaoNfse>' +
           '<' + Prefixo4 + 'Numero>' +
             NumeroNFse +
           '</' + Prefixo4 + 'Numero>' +
           '<' + Prefixo4 + 'CpfCnpj>' +
            '<' + Prefixo4 + 'Cnpj>' +
             Cnpj +
            '</' + Prefixo4 + 'Cnpj>' +
           '</' + Prefixo4 + 'CpfCnpj>' +
           '<' + Prefixo4 + 'InscricaoMunicipal>' +
             IM +
           '</' + Prefixo4 + 'InscricaoMunicipal>' +
           '<' + Prefixo4 + 'CodigoMunicipio>' +
             CodMunicipio +
           '</' + Prefixo4 + 'CodigoMunicipio>' +
          '</' + Prefixo4 + 'IdentificacaoNfse>' +
          '<' + Prefixo4 + 'CodigoCancelamento>' +
            CodCancelamento +
          '</' + Prefixo4 + 'CodigoCancelamento>' +
          '<' + Prefixo4 + 'MotivoCancelamento>' +
            MotivoCancelamento +
          '</' + Prefixo4 + 'MotivoCancelamento>'+
         '</' + Prefixo4 + 'InfPedidoCancelamento>';

   Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgConsSitLoteEquiplano(CodCidade: Integer;
  CNPJ, IM, Protocolo, NumeroLote: String; TagI,
  TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
             '<prestador>' +
               '<nrInscricaoMunicipal>' + IM + '</nrInscricaoMunicipal>' +
               '<cnpj>' + CNPJ + '</cnpj>' +
               '<idEntidade>' + GetIdEntidadeEquiplano(CodCidade) + '</idEntidade>' +
             '</prestador>' +
             '<nrLoteRps>' + NumeroLote + '</nrLoteRps>' +
             //'<nrProtocolo>' + Protocolo + '</nrProtocolo>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgEnviarLoteInfisc(Prefixo3, Prefixo4, Identificador, NameSpaceDad,
  VersaoXML, NumeroLote, CodCidade, CNPJ, IM, RazaoSocial, Transacao, QtdeNotas, ValorTotalServicos,
  ValorTotalDeducoes: String; DataInicial, DataFinal: TDateTime; Notas, TagI,
  TagF: AnsiString): AnsiString;
var
  DadosMsg: AnsiString;
begin
  DadosMsg := Notas;
  Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgConsNFSeInfisc(Prefixo3, Prefixo4, NameSpaceDad, VersaoXML,
  CodCidade, CNPJ, IM, NotaInicial,Serie: String; DataInicial, DataFinal: TDateTime; TagI,
  TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin
// DadosMsg := '<CNPJ>'+CNPJ+'</CNPJ>'+
//             '<chvAcessoNFS-e>'+NotaInicial+'</chvAcessoNFS-e>';

DadosMsg := '<CNPJ>'            + CNPJ                                        + '</CNPJ>'+
            '<notaInicial>'     + NotaInicial                                 + '</notaInicial>' +
            '<notaFinal>'       + NotaInicial                                 + '</notaFinal>' +
            '<emissaoInicial>'  + FormatDateTime( 'yyyy-mm-dd', dataInicial ) + '</emissaoInicial>'+
            '<emissaoFinal>'    + FormatDateTime( 'yyyy-mm-dd', dataFinal )   + '</emissaoFinal>'+
            '<serieNotaFiscal>' + Serie                                       + '</serieNotaFiscal>';

 Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgConsSitLoteInfisc(CodCidade: Integer; CNPJ, IM, Protocolo,
  NumeroLote: String; TagI, TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin
 DadosMsg := '<CNPJ>'+CNPJ+'</CNPJ>'+
             '<cLote>'+Protocolo+'</cLote>';
 Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgCancelarNFSeInfisc(Prefixo3, Prefixo4,
  NameSpaceDad, VersaoXML, CNPJ, Transacao, CodMunicipio,
  NumeroLote: String; Notas, TagI, TagF: AnsiString): AnsiString;
var
 DadosMsg: AnsiString;
begin
 DadosMsg := '<CNPJ>'+CNPJ+'</CNPJ>'+Notas;
 Result := TagI + DadosMsg + TagF;
end;

class function TNFSeG.Gera_DadosMsgEnviarLoteEL(NameSpaceDad, NumeroLote, QtdeRPS, CNPJ,
  IM: String; CodCidade: Integer; OptanteSimples: Boolean; Id, Notas, TagI, TagF: AnsiString): AnsiString;
begin
  Result:= '<LoteRps ' + NameSpaceDad +
            '<Id>' + Id + '</Id>' +
            '<NumeroLote>' + NumeroLote + '</NumeroLote>' +
            '<QuantidadeRps>' + QtdeRPS + '</QuantidadeRps>' +
            '<IdentificacaoPrestador>' +
              '<CpfCnpj>' + CNPJ + '</CpfCnpj>' +
              '<IndicacaoCpfCnpj>' + IfThen(Length(CNPJ)<>14, '1', '2') + '</IndicacaoCpfCnpj>' +
              '<InscricaoMunicipal>' +  IM + '</InscricaoMunicipal>' +
            '</IdentificacaoPrestador>' +
            '<ListaRps>' +
              Notas +
            '</ListaRps>' +
           '</LoteRps>';
{  // #HASH# = é alterado por StringReplace() posteriormente em TNFSeEnviarLoteRPS.Executar
  Result:= TagI +
              '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
              '<hashIdentificador>' + '#HASH#' + '</hashIdentificador>' +
              '<arquivo>' +
               '<Id>' + Id + '</Id>' +
               '<NumeroLote>' + NumeroLote + '</NumeroLote>' +
               '<QuantidadeRps>' + QtdeRPS + '</QuantidadeRps>' +
               '<IdentificacaoPrestador>' +
                 '<CpfCnpj>' + CNPJ + '</CpfCnpj>' +
                 '<IndicacaoCpfCnpj>' + IfThen(Length(CNPJ)<>14, '1', '2') + '</IndicacaoCpfCnpj>' +
                 '<InscricaoMunicipal>' +  IM + '</InscricaoMunicipal>' +
               '</IdentificacaoPrestador>' +
               '<ListaRps>' +
                 Notas +
               '</ListaRps>' +
              '</arquivo>' +
           TagF;}
end;

class function TNFSeG.Gera_DadosMsgConsSitLoteEL(CodCidade: Integer;
  CNPJ, IM, Protocolo, NumeroLote: String; TagI,
  TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
              '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
              '<numeroProtocolo>' + Protocolo + '</numeroProtocolo>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgConsLoteEL(CodCidade: Integer;
  CNPJ, IM, Protocolo, NumeroLote: String; TagI, TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
              '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
              '<numeroProtocolo>' + Protocolo + '</numeroProtocolo>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgConsNFSeRPSEL(CodCidade: Integer;
  NumeroRps, CNPJ, IM: String; TagI, TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
              '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
              '<identificacaoRps>' + NumeroRps + '</identificacaoRps>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgConsNFSeEL(Prefixo3, Prefixo4, NameSpaceDad, VersaoXML,
  NumeroNFSe, CNPJ, IM, CNPJTomador, CNPJITermediarioServ: String; DataInicial,
  DataFinal: TDateTime; TagI, TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
              '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
              '<numeroNfse>' + NumeroNFSe + '</numeroNfse>' +
              '<dataInicial>' + FormatDateTime('yyyy-mm-dd', DataInicial) + '</dataInicial>' +
              '<dataFinal>' + FormatDateTime('yyyy-mm-dd', DataFinal) + '</dataFinal>' +
              '<identificacaoTomador>' + CNPJTomador + '</identificacaoTomador>' +
              '<identificacaoItermediarioServico>' + CNPJITermediarioServ + '</identificacaoItermediarioServico>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgCancelarNFSeEL(
  CodCidade: Integer; CNPJ, IM, NumeroNFSe, MotivoCancelamento: String;
  TagI, TagF: AnsiString): AnsiString;
begin
  Result:= TagI +
              '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
              '<numeroNfse>' + NumeroNFSe + '</numeroNfse>' +
           TagF;
end;

class function TNFSeG.Gera_DadosMsgConsSeqRPSEL(TagI, TagF: AnsiString;
  VersaoXML, CodCidade, IM, CNPJ, SeriePrestacao: String): AnsiString;
begin
 //consultar sequencial RPS
  Result:= '<wsn:ConsultarUltimaRps>' +
              '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
           '<wsn:ConsultarUltimaRps>';
end;

end.
