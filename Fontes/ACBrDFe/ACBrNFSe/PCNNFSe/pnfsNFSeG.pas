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
  SysUtils, Classes, Forms, StrUtils, DateUtils,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnAuxiliar, pcnGerador, pcnConversao,
  pnfsNFSe, pnfsConversao, ACBrUtil;

type

  TNFSeG = class(TPersistent)
  private
    aVersao: String;
    aIdentificador: String;
    aNameSpace: String;
    IdLote: String;

    FGerador: TGerador;

    FProvedor: TnfseProvedor;
    FVersaoNFSe: TVersaoNFSe;

    // Layout - ABRASF
    FPrefixo3: String;
    FPrefixo4: String;
    FIdentificador: String;
    FNameSpaceDad: String;
    FVersaoDados: String;
    FNumeroLote: String;
    FCNPJ: String;
    FIM: String;
    FQtdeNotas: Integer;
    FNotas: String;

    FProtocolo: String;

    FSenha: String;
    FFraseSecreta: String;
    FRazaoSocial: String;

    FNumeroRps: String;
    FSerieRps: String;
    FTipoRps: String;

    FDataInicial: TDateTime;
    FDataFinal: TDateTime;
    FNumeroNFSe: String;
    FSerieNFSe: String;
    FPagina: Integer;
    FCNPJTomador: String;
    FIMTomador: String;
    FCNPJInter: String;
    FIMInter: String;
    FNomeInter: String;

    FCodMunicipio: Integer;
    FCodigoCanc: String;
    FMotivoCanc: String;

    // Layout - ISSDSF
    FVersaoXML: String;
    FTransacao: Boolean;
    FSeriePrestacao: String;
    FValorTotalServicos: Currency;
    FValorTotalDeducoes: Currency;

    // Layout - Equiplano
    FOptanteSimples: TnfseSimNao;

    FPossuiAlertas: Boolean;

    procedure SetAtributos;
    function GetIdEntidadeEquiplano(const IBGE: Integer): String;
    procedure SetCNPJ(const Value: String);
    procedure SetCNPJTomador(const Value: String);
    procedure SetCNPJInter(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;

    function Gera_DadosMsgEnviarLote: String;
    function Gera_DadosMsgConsSitLote: String;
    function Gera_DadosMsgConsLote: String;
    function Gera_DadosMsgConsNFSeRPS: String;
    function Gera_DadosMsgConsNFSe: String;
    function Gera_DadosMsgCancelarNFSe: String;
    function Gera_DadosMsgGerarNFSe: String;
    function Gera_DadosMsgEnviarSincrono: String;
    function Gera_DadosMsgSubstituirNFSe: String;
  published
    property Gerador: TGerador read FGerador write FGerador;

    property Provedor: TnfseProvedor read FProvedor   write FProvedor;
    property VersaoNFSe: TVersaoNFSe read FVersaoNFSe write FVersaoNFSe;

    // Layout - ABRASF
    property Prefixo3: String      read FPrefixo3      write FPrefixo3;
    property Prefixo4: String      read FPrefixo4      write FPrefixo4;
    property Identificador: String read FIdentificador write FIdentificador;
    property NameSpaceDad: String  read FNameSpaceDad  write FNameSpaceDad;
    property VersaoDados: String   read FVersaoDados   write FVersaoDados;
    property NumeroLote: String    read FNumeroLote    write FNumeroLote;
    property CNPJ: String          read FCNPJ          write SetCNPJ;
    property IM: String            read FIM            write FIM;
    property QtdeNotas: Integer    read FQtdeNotas     write FQtdeNotas;
    property Notas: String         read FNotas         write FNotas;

    property Protocolo: String     read FProtocolo     write FProtocolo;

    property Senha: String         read FSenha         write FSenha;
    property FraseSecreta: String  read FFraseSecreta  write FFraseSecreta;
    property RazaoSocial: String   read FRazaoSocial   write FRazaoSocial;

    property NumeroRps: String     read FNumeroRps     write FNumeroRps;
    property SerieRps: String      read FSerieRps      write FSerieRps;
    property TipoRps: String       read FTipoRps       write FTipoRps;

    property DataInicial: TDateTime read FDataInicial write FDataInicial;
    property DataFinal: TDateTime   read FDataFinal   write FDataFinal;
    property NumeroNFSe: String     read FNumeroNFSe  write FNumeroNFSe;
    property SerieNFSe: String      read FSerieNFSe   write FSerieNFSe;
    property Pagina: Integer        read FPagina      write FPagina;
    property CNPJTomador: String    read FCNPJTomador write SetCNPJTomador;
    property IMTomador: String      read FIMTomador   write FIMTomador;
    property CNPJInter: String      read FCNPJInter   write SetCNPJInter;
    property IMInter: String        read FIMInter     write FIMInter;
    property NomeInter: String      read FNomeInter   write FNomeInter;

    property CodMunicipio: Integer read FCodMunicipio write FCodMunicipio;
    property CodigoCanc: String    read FCodigoCanc   write FCodigoCanc;
    property MotivoCanc: String    read FMotivoCanc   write FMotivoCanc;

    // Layout - ISSDSF
    property VersaoXML: String            read FVersaoXML          write FVersaoXML;
    property Transacao: Boolean           read FTransacao          write FTransacao;
    property SeriePrestacao: String       read FSeriePrestacao     write FSeriePrestacao;
    property ValorTotalServicos: Currency read FValorTotalServicos write FValorTotalServicos;
    property ValorTotalDeducoes: Currency read FValorTotalDeducoes write FValorTotalDeducoes;

    // Layout - Equiplano
    property OptanteSimples: TnfseSimNao read FOptanteSimples write FOptanteSimples;

    property PossuiAlertas: Boolean read FPossuiAlertas write FPossuiAlertas;
   end;

implementation

(*
class function TNFSeG2.Gera_DadosMsgConsSeqRPSDSF(TagI, TagF: AnsiString;
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

class function TNFSeG2.Gera_DadosMsgConsSeqRPSEL(TagI, TagF: AnsiString;
  VersaoXML, CodCidade, IM, CNPJ, SeriePrestacao: String): AnsiString;
begin
 //consultar sequencial RPS
  Result:= '<wsn:ConsultarUltimaRps>' +
              '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
           '<wsn:ConsultarUltimaRps>';
end;
*)

{ TNFSeG }

constructor TNFSeG.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TNFSeG.Destroy;
begin
  FGerador.Free;
  inherited;
end;

procedure TNFSeG.SetAtributos;
begin
  // Atributo versao ===========================================================
  if VersaoDados <> '' then
  begin
    if Provedor in [proFintelISS] then
      aVersao := ' Versao="' + VersaoDados + '"'
    else
      aVersao := ' versao="' + VersaoDados + '"';

    if Provedor in [proAbaco, proBetha, proDBSeller, proGinfes, proGoiania,
                    proGovBR, proIssCuritiba, proISSNET, proLexsom, proNatal,
                    proTinus, proRecife, proRJ, proSimplISS, proThema, proTiplan,
                    proAgili, proFISSLex, proSpeedGov, proPronim, proSalvador,
                    proSJP] then
      aVersao := '';
  end
  else
    aVersao := '';

  // Atributo NameSapce ========================================================
  if Provedor = proSimplISS then
    aNameSpace := ' ' + NameSpaceDad
  else
    aNameSpace := '';

  // Valor do atributo Id ======================================================
  case Provedor of
    proTecnos: IdLote := '1' + IntToStrZero(YearOf(Date), 4) +
                         Copy(Notas, Pos('<InfDeclaracaoPrestacaoServico Id="', Notas) + 40, 14) +
                         IntToStrZero(StrToIntDef(NumeroLote, 1), 16);

  else IdLote := NumeroLote;
  end;

  // Atributo Id ===============================================================
  if Identificador <> '' then
  begin
    aIdentificador := ' ' + Identificador + '="' + IdLote + '"';

    if Provedor in [proGovBR, proPronim, proISSDigital] then
      aIdentificador := '';
  end
  else
    aIdentificador := '';

  // Redefine o Profixo 3 ======================================================
  if Provedor = proBetha then
    Prefixo3 := '';
end;

function TNFSeG.GetIdEntidadeEquiplano(const IBGE: Integer): String;
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

procedure TNFSeG.SetCNPJ(const Value: String);
begin
  FCNPJ := OnlyNumber(Value);
end;

procedure TNFSeG.SetCNPJTomador(const Value: String);
begin
  FCNPJTomador := OnlyNumber(Value);
end;

procedure TNFSeG.SetCNPJInter(const Value: String);
begin
  FCNPJInter := OnlyNumber(Value);
end;

function TNFSeG.Gera_DadosMsgEnviarLote: String;
var
  tagCabecalhoCodigoMunicipio: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proInfisc: begin
                 Gerador.ArquivoFormatoXML := Notas;
               end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('lote');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrLote', 01, 15, 1, NumeroLote, '');
                    Gerador.wCampoNFSe(tcInt, '#1', 'qtRps', 01, 14, 1, QtdeNotas, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrVersaoXml', 01, 14, 1, VersaoXML, '');
                    Gerador.wGrupoNFSe('pretador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrCnpj', 01, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 14, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'isOptanteSimplesNacional', 01, 14, 1, SimNaoToStr(OptanteSimples), '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 14, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/pretador');

                    Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                                 '<listaRps>' +
                                                   Notas +
                                                 '</listaRps>';

                    Gerador.wGrupoNFSe('/lote');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wGrupoNFSe('LoteRps' + aNameSpace);
             Gerador.wCampoNFSe(tcStr, '#1', 'Id', 01, 15, 1, IdLote, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 14, 1, NumeroLote, '');
             Gerador.wCampoNFSe(tcInt, '#1', 'QuantidadeRps', 01, 14, 1, QtdeNotas, '');
             Gerador.wGrupoNFSe('IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 01, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaocpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 14, 1, IM, '');
             Gerador.wGrupoNFSe('/IdentificacaoPrestador');

             Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<listaRps>' +
                                            Notas +
                                          '</listaRps>';

             Gerador.wGrupoNFSe('/LoteRps');
           end;

    proISSDSF: begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('Cabecalho');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 01, 15, 1, CodCidadeToCodSiafi(CodMunicipio), '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 01, 14, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'RazaoSocialRemetente', 01, 14, 1, RazaoSocial, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'transacao', 01, 14, 1, LowerCase(booltostr(Transacao, True)), '');
                 Gerador.wCampoNFSe(tcDat, '#1', 'dtInicio', 01, 10, 1, DataInicial, '');
                 Gerador.wCampoNFSe(tcDat, '#1', 'dtFim', 01, 10, 1, DataFinal, '');
                 Gerador.wCampoNFSe(tcInt, '#1', 'QtdRPS', 01, 14, 1, QtdeNotas, '');
                 Gerador.wCampoNFSe(tcDe2, '#1', 'ValorTotalServicos', 01, 14, 1, ValorTotalServicos, '');
                 Gerador.wCampoNFSe(tcDe2, '#1', 'ValorTotalDeducoes', 01, 14, 1, ValorTotalDeducoes, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 14, 1, VersaoXML, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'MetodoEnvio', 01, 02, 1, 'WS', '');
                 Gerador.wGrupoNFSe('/Cabecalho');

                 Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                              '<Lote' + aIdentificador + '>' +
                                                Notas +
                                              '</Lote>';
               end;

    proNFSEBrasil: begin
                     tagCabecalhoCodigoMunicipio := RetornarConteudoEntre(Notas, '<CodigoMunicipio>', '</CodigoMunicipio>');
                     tagCabecalhoCodigoMunicipio := ' codMunicipio="' + tagCabecalhoCodigoMunicipio + '"';

                     Gerador.Prefixo := Prefixo3;
                     Gerador.wGrupoNFSe('LoteRps' + tagCabecalhoCodigoMunicipio + aVersao + aIdentificador);

                     Gerador.Prefixo := Prefixo4;
                     Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, IdLote, '');

                     if VersaoNFSe = ve100 then
                     begin
                       Gerador.wGrupoNFSe('CpfCnpj');
                       if Length(OnlyNumber(Cnpj)) <= 11 then
                         Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, Cnpj, '')
                       else
                         Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');
                       Gerador.wGrupoNFSe('/CpfCnpj');
                     end
                     else
                       Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');

                     Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                     Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 15, 1, QtdeNotas, '');

                     Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                                  '<' + Prefixo4 + 'ListaRps>' +
                                                    Notas +
                                                  '</' + Prefixo4 + 'ListaRps>';

                     Gerador.Prefixo := Prefixo3;
                     Gerador.wGrupoNFSe('/LoteRps');
                   end;

  else begin
         Gerador.Prefixo := Prefixo3;
         if Provedor in [proCoplan] then
           Gerador.wGrupoNFSe('LoteRps' + aVersao + aIdentificador)
         else
           Gerador.wGrupoNFSe('LoteRps' + aIdentificador + aVersao + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, IdLote, '');

         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(OnlyNumber(Cnpj)) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');

         Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
         Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 15, 1, QtdeNotas, '');

         Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                      '<' + Prefixo4 + 'ListaRps>' +
                                        Notas +
                                      '</' + Prefixo4 + 'ListaRps>';

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/LoteRps');
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsSitLote: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proInfisc: begin
                 Gerador.Prefixo := '';
                 Gerador.wCampoNFSe(tcStr, '#1', 'CNPJ', 01, 15, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'cLote', 01, 15, 1, Protocolo, '');
               end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('pretador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 14, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrCnpj', 01, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 14, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/pretador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrLoteRps', 01, 14, 1, NumeroLote, '');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 01, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'numeroProtocolo', 01, 14, 1, Protocolo, '');
           end;

    proISSDSF: begin
                 // Não possui 
               end;

  else begin
         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('Prestador' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(OnlyNumber(Cnpj)) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');

         Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/Prestador');

         Gerador.wCampoNFSe(tcStr, '#4', 'Protocolo', 01, 15, 1, Protocolo, '', True, aNameSpace);
       end;
  end;

  if Provedor = proDBSeller then
    Gerador.ArquivoFormatoXML := '<ConsultarSituacaoLoteRpsEnvio>' +
                                   Gerador.ArquivoFormatoXML +
                                 '</ConsultarSituacaoLoteRpsEnvio>';

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, pro4R, proAgili,
                  proCoplan, profintelISS, proFiorilli, proGoiania, proGovDigital,
                  proISSDigital, proISSe, proProdata, proVirtual, proSaatri,
                  proFreire, proPVH, proVitoria, proTecnos, proSisPMJP,
                  proSystemPro] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsLote: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proInfisc: begin
                 // Não Possui 
               end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('pretador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 14, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrCnpj', 01, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 14, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/pretador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrLoteRps', 01, 14, 1, NumeroLote, '');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 01, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'numeroProtocolo', 01, 14, 1, Protocolo, '');
           end;

    proISSDSF: begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('Cabecalho');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 01, 15, 1, CodCidadeToCodSiafi(CodMunicipio), '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 01, 14, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 14, 1, VersaoXML, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 14, 1, NumeroLote, '');
                 Gerador.wGrupoNFSe('/Cabecalho');
               end;

  else begin
         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('Prestador' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(OnlyNumber(Cnpj)) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');

         if Provedor = proTecnos then
           Gerador.wCampoNFSe(tcStr, '#3', 'RazaoSocial', 01, 40, 1, RazaoSocial, '');

         Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

         if Provedor = proISSDigital then
         begin
           Gerador.wCampoNFSe(tcStr, '#5', 'Senha', 01, 40, 1, Senha, '');
           Gerador.wCampoNFSe(tcStr, '#6', 'FraseSecreta', 01, 40, 1, FraseSecreta, '');
         end;

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/Prestador');

         Gerador.wCampoNFSe(tcStr, '#4', 'Protocolo', 01, 15, 1, Protocolo, '', True, aNameSpace);
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsNFSeRPS: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proInfisc: begin
                 // Não Possui
               end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('rps');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrRps', 01, 14, 1, NumeroRps, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrEmissorRps', 01, 01, 1, '1', '');
                    Gerador.wGrupoNFSe('/rps');
                    Gerador.wGrupoNFSe('pretador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 14, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrCnpj', 01, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 14, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/pretador');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 01, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoRps', 01, 14, 1, NumeroRps, '');
           end;

    proISSDSF: begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('Cabecalho');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 01, 15, 1, CodCidadeToCodSiafi(CodMunicipio), '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 01, 14, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'transacao', 01, 14, 1, LowerCase(booltostr(Transacao, True)), '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 14, 1, VersaoXML, '');
                 Gerador.wGrupoNFSe('/Cabecalho');

                 Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                              '<Lote' + aIdentificador + '>' +
                                                Notas +
                                              '</Lote>';
               end;

  else begin
         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('IdentificacaoRps' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 40, 1, NumeroRps, '');
         Gerador.wCampoNFSe(tcStr, '#2', 'Serie', 01, 40, 1, SerieRps, '');
         Gerador.wCampoNFSe(tcStr, '#3', 'Tipo', 01, 40, 1, TipoRps, '');

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/IdentificacaoRps');

         Gerador.wGrupoNFSe('Prestador' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(OnlyNumber(Cnpj)) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');

         if Provedor = proTecnos then
           Gerador.wCampoNFSe(tcStr, '#3', 'RazaoSocial', 01, 40, 1, RazaoSocial, '');

         Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

         if Provedor = proISSDigital then
         begin
           Gerador.wCampoNFSe(tcStr, '#5', 'Senha', 01, 40, 1, Senha, '');
           Gerador.wCampoNFSe(tcStr, '#6', 'FraseSecreta', 01, 40, 1, FraseSecreta, '');
         end;

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/Prestador');
       end;
  end;

  if Provedor = proDBSeller then
    Gerador.ArquivoFormatoXML := '<ConsultarNfseRpsEnvio>' +
                                   Gerador.ArquivoFormatoXML +
                                 '</ConsultarNfseRpsEnvio>';

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsNFSe: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proInfisc: begin
                 Gerador.Prefixo := '';
                 Gerador.wCampoNFSe(tcStr, '#1', 'CNPJ', 01, 15, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'notaInicial', 01, 15, 1, NumeroNFSe, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'notafinal', 01, 15, 1, NumeroNFSe, '');
                 Gerador.wCampoNFSe(tcDat, '#1', 'emissaoInicial', 01, 15, 1, DataInicial, '');
                 Gerador.wCampoNFSe(tcDat, '#1', 'emissaofinal', 01, 15, 1, DataFinal, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'serieNotaFiscal', 01, 15, 1, SerieNFSe, '');
               end;

    proEquiplano: begin
                    // Nao Possui
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 01, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'numeroNfse', 01, 14, 1, NumeroNFSe, '');
             Gerador.wCampoNFSe(tcDat, '#1', 'dataInicial', 01, 10, 1, DataInicial, '');
             Gerador.wCampoNFSe(tcDat, '#1', 'dataFinal', 01, 10, 1, DataFinal, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoTomador', 01, 14, 1, CNPJTomador, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoIntermediarioServico', 01, 14, 1, CNPJInter, '');
           end;

    proISSDSF: begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('Cabecalho');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 01, 15, 1, CodCidadeToCodSiafi(CodMunicipio), '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 01, 14, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipalPrestador', 01, 14, 1, IM, '');
                 Gerador.wCampoNFSe(tcDat, '#1', 'dtInicio', 01, 10, 1, DataInicial, '');
                 Gerador.wCampoNFSe(tcDat, '#1', 'dtFim', 01, 10, 1, DataFinal, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'NotaInicial', 01, 14, 1, NumeroNFSe, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 14, 1, VersaoXML, '');
                 Gerador.wGrupoNFSe('/Cabecalho');
               end;

  else begin
         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('Prestador' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(OnlyNumber(Cnpj)) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');

         if Provedor = proTecnos then
           Gerador.wCampoNFSe(tcStr, '#3', 'RazaoSocial', 01, 40, 1, RazaoSocial, '');

         Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

         if Provedor = proISSDigital then
         begin
           Gerador.wCampoNFSe(tcStr, '#5', 'Senha', 01, 40, 1, Senha, '');
           Gerador.wCampoNFSe(tcStr, '#6', 'FraseSecreta', 01, 40, 1, FraseSecreta, '');
         end;

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/Prestador');

         if NumeroNFSe <> '' then
         begin
           if Provedor in [proPVH, proSystemPro, proPublica, proSisPMJP] then
           begin
             Gerador.wGrupoNFSe('Faixa');
             Gerador.wCampoNFSe(tcStr, '#5', 'NumeroNfseInicial', 01, 40, 1, NumeroNFSe, '');
             Gerador.wCampoNFSe(tcStr, '#6', 'NumeroNfseFinal', 01, 40, 1, NumeroNFSe, '');
             Gerador.wGrupoNFSe('/Faixa');
           end
           else
             Gerador.wCampoNFSe(tcStr, '#5', 'NumeroNfse', 01, 40, 1, NumeroNFSe, '');
         end;

         if (DataInicial>0) and (DataFinal>0) then
         begin
           Gerador.wGrupoNFSe('PeriodoEmissao');
           Gerador.wCampoNFSe(tcDat, '#5', 'DataInicial', 01, 10, 1, DataInicial, '');
           Gerador.wCampoNFSe(tcDat, '#6', 'DataFinal', 01, 10, 1, DataFinal, '');
           Gerador.wGrupoNFSe('/PeriodoEmissao');
         end;

         if (CNPJTomador <> '') or (IMTomador <> '')then
         begin
           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('Tomador');

           Gerador.Prefixo := Prefixo4;
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(OnlyNumber(CnpjTomador)) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, CnpjTomador, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, CnpjTomador, '');
           Gerador.wGrupoNFSe('/CpfCnpj');

           Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IMTomador, '');

           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('/Tomador');
         end;

         if (NomeInter <> '') and (CNPJInter <> '') then
         begin
           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('IntermediarioServico');

           Gerador.Prefixo := Prefixo4;
           Gerador.wCampoNFSe(tcStr, '#4', 'RazaoSocial', 01, 15, 1, NomeInter, '');

           if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
           begin
             Gerador.wGrupoNFSe('CpfCnpj');
             if Length(OnlyNumber(CnpjInter)) <= 11 then
               Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, CnpjInter, '')
             else
               Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, CnpjInter, '');
             Gerador.wGrupoNFSe('/CpfCnpj');
           end
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, CnpjInter, '');

           Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IMInter, '');

           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('/IntermediarioServico');
         end;

         if Provedor in [proFiorilli, profintelISS, proPVH, proSystemPro,
                         proSisPMJP, proDigifred] then
           Gerador.wCampoNFSe(tcInt, '#4', 'Pagina', 01, 5, 1, Pagina, '');
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgCancelarNFSe: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proInfisc: begin
                 Gerador.Prefixo := '';
                 Gerador.wCampoNFSe(tcStr, '#1', 'CNPJ', 01, 15, 1, Cnpj, '');
                 Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
               end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('pretador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 14, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrCnpj', 01, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 14, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/pretador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrNfse', 01, 14, 1, NumeroNFSe, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'dsMotivoCancelamento', 01, 01, 1, MotivoCanc, '');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 01, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'numeroNfse', 01, 14, 1, NumeroNFSe, '');
           end;

    proISSDSF: begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('Cabecalho');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 01, 15, 1, CodCidadeToCodSiafi(CodMunicipio), '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 01, 14, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'transacao', 01, 14, 1, LowerCase(booltostr(Transacao, True)), '');
                 Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 14, 1, VersaoXML, '');
                 Gerador.wGrupoNFSe('/Cabecalho');

                 Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                              '<Lote' + aIdentificador + '>' +
                                                Notas +
                                              '</Lote>';
               end;

    proGinfes: begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('Prestador');

                 Gerador.Prefixo := Prefixo4;
                 Gerador.wCampoNFSe(tcStr, '#1', 'Cnpj', 01, 14, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#2', 'InscricaoMunicipal', 01, 40, 1, IM, '');

                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('/Prestador');
                 Gerador.wCampoNFSe(tcStr, '#3', 'NumeroNfse', 01, 40, 1, NumeroNfse, '');
               end;

    proIssCuritiba: begin
                      Gerador.Prefixo := '';
                      Gerador.wGrupoNFSe('InfPedidoCancelamento');
                      Gerador.wGrupoNFSe('IdentificacaoNfse');
                      Gerador.wCampoNFSe(tcStr, '#1', 'Cnpj', 01, 14, 1, Cnpj, '');
                      Gerador.wCampoNFSe(tcStr, '#2', 'InscricaoMunicipal', 01, 40, 1, IM, '');
                      Gerador.wCampoNFSe(tcStr, '#3', 'Numero', 01, 40, 1, NumeroNfse, '');
                      Gerador.wGrupoNFSe('/IdentificacaoNfse');
                      Gerador.wGrupoNFSe('/InfPedidoCancelamento');
                    end;
  else begin
         Gerador.Prefixo := Prefixo4;
         Gerador.wGrupoNFSe('IdentificacaoNfse');
         Gerador.wCampoNFSe(tcStr, '#3', 'Numero', 01, 40, 1, NumeroNfse, '');

         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(OnlyNumber(Cnpj)) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');

         Gerador.wCampoNFSe(tcStr, '#2', 'InscricaoMunicipal', 01, 40, 1, IM, '');
         Gerador.wCampoNFSe(tcInt, '#2', 'CodigoMunicipio', 01, 40, 1, CodMunicipio, '');

         Gerador.wGrupoNFSe('/IdentificacaoNfse');

         Gerador.wCampoNFSe(tcStr, '#1', 'CodigoCancelamento', 01, 14, 1, CodigoCanc, '');

         if (Provedor in [proPublica, proTecnos]) and (MotivoCanc <> '') then
           Gerador.wCampoNFSe(tcStr, '#1', 'MotivoCancelamento', 01, 14, 1, MotivoCanc, '');

         Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                      '</' + Prefixo4 + 'InfPedidoCancelamento>';
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  if Provedor = proDBSeller then
    Result := '<CancelarNfse>' + Result + '</CancelarNfse>';

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgGerarNFSe: String;
var
  tagCabecalhoCodigoMunicipio: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proInfisc: begin
                 // Nao Possui
               end;

    proEquiplano: begin
                    // Nao Possui
                  end;

    proEL: begin
             // Nao Possui
           end;

    proISSDSF: begin
                 // Nao Possui
               end;

    proWebISS: begin
                 Gerador.wGrupoNFSe('LoteRps' + aIdentificador + aVersao);

                 Gerador.Prefixo := Prefixo4;
                 Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, IdLote, '');

                 if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
                 begin
                   Gerador.wGrupoNFSe('CpfCnpj');
                   if Length(OnlyNumber(Cnpj)) <= 11 then
                     Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 01, 11, 1, Cnpj, '')
                   else
                     Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');
                   Gerador.wGrupoNFSe('/CpfCnpj');
                 end
                 else
                   Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 01, 14, 1, Cnpj, '');

                 Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                 Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 15, 1, QtdeNotas, '');

                 Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                              '<' + Prefixo4 + 'ListaRps>' +
                                                Notas +
                                              '</' + Prefixo4 + 'ListaRps>';

                 Gerador.Prefixo := Prefixo3;
                 Gerador.wGrupoNFSe('/LoteRps');

                 Result := Gerador.ArquivoFormatoXML;
               end;

  else Result := Notas;
  end;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proAbaco, proBetha,
                  proBetim, proBHIss, proDBSeller, proEquiplano, proFIssLex,
                  proGinfes, proGovBR, proIssCuritiba, proIssIntel, proIssNet,
                  proLexsom, proNatal, proTinus, proProdemge, proRJ, proSimplIss,
                  proThema, proTiplan, proIssDSF, proInfisc, proAgili,
                  proSpeedGov, proPronim, proActcon, proSalvador,
                  proNFSEBrasil] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgEnviarSincrono: String;
begin
  Result := Gera_DadosMsgEnviarLote;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proAbaco, proBetha,
                  proBetim, proBHISS, proDBSeller, proEquiplano, profintelISS,
                  proFISSLex, proGinfes, proGoiania, proGovBR, proIssCuritiba,
                  proISSDigital, proISSIntel, proISSNet, proLexsom, proNatal,
                  proTinus, proProdemge, proPublica, proRecife, proRJ, proSaatri,
                  proFreire, proSimplISS, proThema, proTiplan, proWebISS,
                  proProdata, proAgili, proSpeedGov, proPronim, proSalvador,
                  proNFSEBrasil] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgSubstituirNFSe: String;
begin
  Result := Gera_DadosMsgCancelarNFSe + '</' + Prefixo3 + 'Pedido>' + Notas;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum] then
    Result := '';
end;

end.
