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
    FIdLote: String;

    // Layout - ISSDSF
    FVersaoXML: String;
    FTransacao: Boolean;
    FSeriePrestacao: String;
    FValorTotalServicos: Currency;
    FValorTotalDeducoes: Currency;

    // Layout - CONAM
    FUserWeb: String;
    FSenhaWeb: String;
    FTipoTributacao: String;
    FQtdTributos: Integer; //almp1
    FValorNota: Currency;
    FAliquotaISS: Currency;
    FValorIss: Currency;
    FValorIssRetido: Currency;
    FValorTotalTributos: Currency; //almp1
    FDataOptanteSimples: TDateTime; //almp1
    FExigibilidadeISS: TnfseExigibilidadeISS; //almp1
    FRegimeEspecialTributacao: TnfseRegimeEspecialTributacao; //almp1
    // Layout - Equiplano
    FOptanteSimples: TnfseSimNao;

    // Layout - Governa
    FChaveAcessoPrefeitura: String;
    FCodVerificacaoRPS: String;

    // Layout - SP
    FAssinaturaCan: String;

    FPossuiAlertas: Boolean;

    // Layout - Agili
    FCNPJPrefeitura: String;

    // Layout - EL
    FHashIdent: String;

    procedure SetAtributos;
    function GetIdEntidadeEquiplano(const IBGE: Integer): String;
    procedure SetCNPJ(const Value: String);
    procedure SetCNPJTomador(const Value: String);
    procedure SetCNPJInter(const Value: String);
    procedure SetCNPJPrefeitura(const Value: String);
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

    function Gera_DadosMsgAbrirSessao: String;
    function Gera_DadosMsgFecharSessao: String;
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

    // Layout - CONAM
    property UserWeb: String          read FUserWeb        write FUserWeb;
    property SenhaWeb: String         read FSenhaWeb       write FSenhaWeb;
    property TipoTributacao: String   read FTipoTributacao write FTipoTributacao;
    property QtdTributos: Integer     read FQtdTributos    write FQtdTributos;
    property ValorNota: Currency      read FValorNota      write FValorNota;
    property AliquotaIss: Currency    read FAliquotaIss    write FAliquotaIss;
    property ValorIss: Currency       read FValorIss       write FValorIss;
    property ValorIssRetido: Currency read FValorIssRetido write FValorIssRetido;
    property ValorTotalTributos: Currency read FValorTotalTributos write FValorTotalTributos; //almp1
    property DataOptanteSimples: TDateTime read FDataOptanteSimples write FDataOptanteSimples; //almp1
    property ExigibilidadeISS: TnfseExigibilidadeISS read FExigibilidadeISS write FExigibilidadeISS; //almp1
    property RegimeEspecialTributacao: TnfseRegimeEspecialTributacao read FRegimeEspecialTributacao write FRegimeEspecialTributacao; //almp1

    // Layout - Equiplano
    property OptanteSimples: TnfseSimNao read FOptanteSimples write FOptanteSimples;

    // Layout - Governa
    // Layout - CTA campo "ChaveAcessoPrefeitura" equivale ao "TokenEnvio"
    property ChaveAcessoPrefeitura: String read FChaveAcessoPrefeitura write FChaveAcessoPrefeitura;
    property CodVerificacaoRPS: String read FCodVerificacaoRPS write FCodVerificacaoRPS;

    // Layout - Agili
    property CNPJPrefeitura: String read FCNPJPrefeitura write SetCNPJPrefeitura;

    // Layout - SP
    property AssinaturaCan: String read FAssinaturaCan write FAssinaturaCan;

    property PossuiAlertas: Boolean read FPossuiAlertas write FPossuiAlertas;

    // Layout - EL
    property HashIdent: String read FHashIdent write FHashIdent;

    property IdLote: String read FIdLote write FIdLote;
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
var
 dhEnvio: String;
begin
  dhEnvio := DateToStr(Date) + '_' + TimeToStr(Time);

  dhEnvio := StringReplace(dhEnvio, '/', '', [rfReplaceAll]);
  dhEnvio := StringReplace(dhEnvio, ':', '', [rfReplaceAll]);

  // Atributo versao ===========================================================
  if VersaoDados <> '' then
  begin
    if Provedor in [proFintelISS, proSP, proNotaBlu] then
      aVersao := ' Versao="' + VersaoDados + '"'
    else
      aVersao := ' versao="' + VersaoDados + '"';

    if Provedor in [proAbaco, proBetha, proDBSeller, proGinfes, proGoiania,
                    proGovBR, proIssCuritiba, proISSNET, proLexsom, proNatal,
                    proTinus, proRecife, proRJ, proSimplISS, proThema, proTiplan,
                    proAgiliv2, proFISSLex, proSpeedGov, proPronim, proSalvador,
                    proSJP, proWebISS, proSystemPro] then
      aVersao := '';
  end
  else
    aVersao := '';

  // Atributo NameSapce ========================================================
  if Provedor in [proEL, proSimplISS] then
    aNameSpace := ' ' + NameSpaceDad
  else
    aNameSpace := '';

  // Valor do atributo Id ======================================================
  case Provedor of
//    proAbaco: IdLote := 'LOTE' + NumeroLote;

    proBethav2: IdLote := 'lote' + NumeroLote;

    proEL: begin
             IdLote := StringOfChar('0', 15) + OnlyNumber(NumeroRps) + SerieRps;
             IdLote := copy(IdLote, length(IdLote) - 15 + 1, 15);
           end;

    proSalvador: IdLote := 'Lote' + NumeroLote;

    proSiam: IdLote := 'Lote_' + NumeroLote + '_' + dhEnvio;

    proTecnos: IdLote := '1' + IntToStrZero(YearOf(Date), 4) + CNPJ +
                         IntToStrZero(StrToIntDef(NumeroLote, 1), 16);

    proWebISS,
    proWebISSv2: IdLote := 'Lote' + CNPJ + IM + NumeroLote;
  else
    IdLote := NumeroLote;
  end;

  // Atributo Id ===============================================================
  if Identificador <> '' then
  begin
    aIdentificador := ' ' + Identificador + '="' + IdLote + '"';

    if Provedor in [proGovBR, proPronim{, proISSDigital}] then
      aIdentificador := '';
  end
  else
    aIdentificador := '';

  // Redefine o Profixo 3 ======================================================
  if Provedor in [proBetha, proBethav2] then
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
    4119608: Result:= '104';// Pitanga/PR
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

procedure TNFSeG.SetCNPJPrefeitura(const Value: String);
begin
  FCNPJPrefeitura := OnlyNumber(Value);
end;

function TNFSeG.Gera_DadosMsgEnviarLote: String;
var
  Atributo_cMun: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proAgili,
    proAgiliv2: begin
                  if VersaoNFSe = ve100 then
                    Gerador.wCampoNFSe(tcStr, '#1', 'UnidadeGestora', 14, 14, 1, CNPJPrefeitura, '');

                  Gerador.Prefixo := Prefixo3;
                  //Gerador.wGrupoNFSe('LoteRps' + aIdentificador + aVersao + aNameSpace);
                  Gerador.wGrupoNFSe('LoteRps');

                  Gerador.Prefixo := Prefixo4;
                  Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');

                  if VersaoNFSe = ve100 then
                  begin
                    Gerador.wGrupoNFSe('IdentificacaoPrestador');
                    Gerador.wCampoNFSe(tcStr, '' , 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');
                  end;

                  Gerador.wGrupoNFSe('CpfCnpj');

                  if Length(Cnpj) <= 11 then
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
                  else
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

                  Gerador.wGrupoNFSe('/CpfCnpj');
                  Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');

                  if VersaoNFSe = ve100 then
                    Gerador.wGrupoNFSe('/IdentificacaoPrestador');

                  Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

                  Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                               '<' + Prefixo4 + 'ListaRps>' +
                                                 Notas +
                                               '</' + Prefixo4 + 'ListaRps>';

                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupoNFSe('/LoteRps');

                  if VersaoNFSe = ve100 then
                    Gerador.wCampoNFSe(tcStr, '#5', 'Versao', 4, 4, 1, '1.00', '');
                end;

    proCONAM: begin
                Gerador.Opcoes.DecimalChar := ',';
                Gerador.Prefixo := '';
                Gerador.wGrupoNFSe('Sdt_processarpsin xmlns="NFe"');
                Gerador.wGrupoNFSe('Login');
                Gerador.wGrupoNFSe('CodigoUsuario>' + UserWeb + '</CodigoUsuario');
                Gerador.wGrupoNFSe('CodigoContribuinte>' + SenhaWeb + '</CodigoContribuinte');
                Gerador.wGrupoNFSe('/Login');

                // Identificaçao do RPS
                Gerador.wGrupoNFSe('SDTRPS');
                Gerador.wCampoNFSe(tcStr, '', 'Ano'    , 01, 04, 0, FormatDateTime('yyyy', DataInicial) , '');
                Gerador.wCampoNFSe(tcStr, '', 'Mes'    , 01, 02, 0, FormatDateTime('mm', DataInicial) , '');
                Gerador.wCampoNFSe(tcStr, '', 'CPFCNPJ', 01, 14, 0, CNPJ , '');
                Gerador.wCampoNFSe(tcStr, '', 'DTIni'  , 01, 10, 0, FormatDateTime('dd/mm/yyyy', DataInicial) , '');
                Gerador.wCampoNFSe(tcStr, '', 'DTFin'  , 01, 10, 0, FormatDateTime('dd/mm/yyyy', DataFinal) , '');

                if OptanteSimples = snSim then
                begin
                  Gerador.wCampoNFSe(tcInt, '', 'TipoTrib'   , 01, 01, 0, 4 , '');
                  // Data de adesao ao simples nacional
                  Gerador.wCampoNFSe(tcStr, '', 'DtAdeSN'    , 01, 10, 0, FormatDateTime('dd/mm/yyyy', DataOptanteSimples) , '');
                  Gerador.wCampoNFSe(tcDe2, '', 'AlqIssSN_IP', 01, 06, 0, AliquotaIss, '');
                end
                else begin
                  case ExigibilidadeISS of
                    exiExigivel:                       Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 1 , '');
                    exiNaoIncidencia,
                    exiIsencao,
                    exiImunidade:                      Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 2 , '');
                    exiSuspensaDecisaoJudicial,
                    exiSuspensaProcessoAdministrativo: Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 3 , '');
                    exiExportacao:                     Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 5 , '');
                  end;
                  // Data de adesao ao simples nacional
                  Gerador.wCampoNFSe(tcStr, '', 'DtAdeSN'    , 01, 10, 0, '', '');
                  Gerador.wCampoNFSe(tcStr, '', 'AlqIssSN_IP', 01, 06, 0, '' , '');
                end;

                if RegimeEspecialTributacao = retMicroempresarioIndividual then
                  Gerador.wCampoNFSe(tcStr, '', 'AlqIssSN_IP', 001, 6, 0, '' , '');

                Gerador.wCampoNFSe(tcStr, '', 'Versao', 001, 4, 0, '2.00' , '');

                Gerador.wGrupoNFSe('Reg20');
                Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                Gerador.wGrupoNFSe('/Reg20');

                // Inicio do rodape registro 90
                Gerador.wGrupoNFSe('Reg90');
                Gerador.wCampoNFSe(tcStr, '', 'QtdRegNormal'  , 01, 05, 1, QtdeNotas, '');
                Gerador.wCampoNFSe(tcDe2, '', 'ValorNFS'      , 01, 16, 2, ValorTotalServicos, '');
                Gerador.wCampoNFSe(tcDe2, '', 'ValorISS'      , 01, 16, 2, ValorIss, '');
                Gerador.wCampoNFSe(tcDe2, '', 'ValorDed'      , 01, 16, 2, ValorTotalDeducoes, '');
                Gerador.wCampoNFSe(tcDe2, '', 'ValorIssRetTom', 01, 16, 2, ValorIssRetido, '');
                Gerador.wCampoNFSe(tcDe2, '', 'ValorTributos' , 01, 16, 2, ValorTotalTributos, '');
                Gerador.wCampoNFSe(tcStr, '', 'QtdReg30'      , 01, 05, 1, QtdTributos, '');
                Gerador.wGrupoNFSe('/Reg90');
                // Fim do rodape registro 90

                Gerador.wGrupoNFSe('/SDTRPS');
                Gerador.wGrupoNFSe('/Sdt_processarpsin');
              end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('lote');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrLote', 01, 15, 1, NumeroLote, '');
                    Gerador.wCampoNFSe(tcInt, '#1', 'qtRps', 01, 14, 1, QtdeNotas, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrVersaoXml', 01, 05, 1, VersaoXML, '');
                    Gerador.wGrupoNFSe('prestador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrCnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'isOptanteSimplesNacional', 01, 14, 1, SimNaoToStr(OptanteSimples), '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/prestador');

                    Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                                 '<listaRps>' +
                                                   Notas +
                                                 '</listaRps>';

                    Gerador.wGrupoNFSe('/lote');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wGrupoNFSe('LoteRps' + aNameSpace);

             Gerador.wCampoNFSe(tcStr, '#1', 'Id', 13, 32, 1, IdLote, ''); // ?? Código Verificação
             Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 14, 1, NumeroLote, '');
             Gerador.wCampoNFSe(tcInt, '#1', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');
             Gerador.wGrupoNFSe('IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ) <> 14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupoNFSe('/IdentificacaoPrestador');

             Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<ListaRps>' +
                                            Notas +
                                          '</ListaRps>';

             Gerador.wGrupoNFSe('/LoteRps');
           end;

    proInfisc,
    proInfiscv11: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('envioLote versao="1.0"');
                    Gerador.wCampoNFSe(tcStr, '', 'CNPJ'   , 01, 14, 1, Cnpj, '');
                    Gerador.wCampoNFSe(tcStr, '', 'dhTrans', 01, 19, 1, FormatDateTime('yyyy-mm-dd hh:mm:ss', Now), '');  {@/\@}
                    Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                    Gerador.wGrupoNFSe('/envioLote');
                  end;

    proISSDSF,
    proCTA: begin
              Gerador.Prefixo := '';
              Gerador.wGrupoNFSe('Cabecalho');
              if Provedor = proCTA then
                Gerador.wCampoNFSe(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'RazaoSocialRemetente', 01, 14, 1, RazaoSocial, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'transacao', 01, 14, 1, LowerCase(booltostr(Transacao, True)), '');
              Gerador.wCampoNFSe(tcDat, '#1', 'dtInicio', 01, 10, 1, DataInicial, '');
              Gerador.wCampoNFSe(tcDat, '#1', 'dtFim', 01, 10, 1, DataFinal, '');
              Gerador.wCampoNFSe(tcInt, '#1', 'QtdRPS', 01, 14, 1, QtdeNotas, '');
              Gerador.wCampoNFSe(tcDe2, '#1', 'ValorTotalServicos', 01, 14, 1, ValorTotalServicos, '');
              Gerador.wCampoNFSe(tcDe2, '#1', 'ValorTotalDeducoes', 01, 14, 1, ValorTotalDeducoes, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'MetodoEnvio', 01, 02, 1, 'WS', '');
              Gerador.wGrupoNFSe('/Cabecalho');

              Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<Lote' + aIdentificador + '>' +
                                            Notas +
                                          '</Lote>';
            end;

    proNFSEBrasil: begin
                     Atributo_cMun := ' codMunicipio="' + IntToStr(CodMunicipio) + '"';

                     Gerador.Prefixo := Prefixo3;
                     Gerador.wGrupoNFSe('LoteRps' + Atributo_cMun + aVersao + aIdentificador);

                     Gerador.Prefixo := Prefixo4;
                     Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');

                     Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

                     Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                     Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

                     Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                                  '<' + Prefixo4 + 'ListaRps>' +
                                                    Notas +
                                                  '</' + Prefixo4 + 'ListaRps>';

                     Gerador.Prefixo := Prefixo3;
                     Gerador.wGrupoNFSe('/LoteRps');
                   end;

    proGoverna: begin
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('LoteRps');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wCampoNFSe(tcStr, '', 'CodCadBic', 01, 15, 1, IM, '');


                  if CodMunicipio = 3104007  then //Araxá
                    Gerador.wCampoNFSe(tcStr, '', 'VrsArq', 01, 01, 1, '4', '')
                  else
                    Gerador.wCampoNFSe(tcStr, '', 'VrsArq', 01, 01, 1, '1', '');

                  Gerador.wCampoNFSe(tcStr, '', 'ChvAcs', 30, 30, 1, ChaveAcessoPrefeitura, '');
                  Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('/LoteRps');
                end;

    proSP,
    proNotaBlu: begin
             Gerador.Opcoes.SuprimirDecimais := True;

             Gerador.wGrupoNFSe('Cabecalho' + aVersao + ' xmlns=""');
             Gerador.wGrupoNFSe('CPFCNPJRemetente');
             Gerador.wCampoCNPJCPF('', '', Cnpj);
             Gerador.wGrupoNFSe('/CPFCNPJRemetente');
//             Gerador.wCampoNFSe(tcStr, '#2', 'CNPJRemetente', 14, 14, 1, Cnpj, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'transacao', 01, 05, 0, LowerCase(BooltoStr(Transacao, True)), '');
             Gerador.wCampoNFSe(tcDat, '#1', 'dtInicio', 01, 10, 1, DataInicial, '');
             Gerador.wCampoNFSe(tcDat, '#1', 'dtFim', 01, 10, 1, DataFinal, '');
             Gerador.wCampoNFSe(tcInt, '#1', 'QtdRPS', 01, 14, 1, QtdeNotas, '');
             Gerador.wCampoNFSe(tcDe2, '#1', 'ValorTotalServicos', 1, 15, 1, ValorTotalServicos, '');
             Gerador.wCampoNFSe(tcDe2, '#1', 'ValorTotalDeducoes', 1, 15, 1, ValorTotalDeducoes, '');
             Gerador.wGrupoNFSe('/Cabecalho');

             Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;

             Gerador.Opcoes.SuprimirDecimais := False;
           end;

     proSMARAPD,
     proIPM:
          begin
            Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas
          end;

     proISSJoinville:
          begin
            Gerador.wGrupoNFSe('LoteRps' + aVersao + aIdentificador);
            Gerador.Prefixo := Prefixo4;
            Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');
            Gerador.wGrupoNFSe('Prestador');
            Gerador.wGrupoNFSe('CpfCnpj');
            if Length(Cnpj) <= 11 then
              Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
            else
              Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
            Gerador.wGrupoNFSe('/CpfCnpj');
            Gerador.wGrupoNFSe('/Prestador');
            Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');
            Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                       '<' + Prefixo4 + 'ListaRps>' +
                                         Notas +
                                       '</' + Prefixo4 + 'ListaRps>';
            Gerador.Prefixo := Prefixo3;
            Gerador.wGrupoNFSe('/LoteRps');
          end;

     proELv2,
     proSmarAPDABRASF:
          begin
           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('LoteRps' + aIdentificador + aVersao + aNameSpace);
           Gerador.Prefixo := Prefixo4;
           Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');
           Gerador.wGrupoNFSe('Prestador');
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(Cnpj) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
           Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
           Gerador.wGrupoNFSe('/Prestador');
           Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

           Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                        '<' + Prefixo4 + 'ListaRps>' +
                                          Notas +
                                        '</' + Prefixo4 + 'ListaRps>';

           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('/LoteRps');
          end;

  else begin
         Gerador.Prefixo := Prefixo3;
         if Provedor in [proCoplan, proSIAPNet] then
           Gerador.wGrupoNFSe('LoteRps' + aVersao + aIdentificador)
         else
           Gerador.wGrupoNFSe('LoteRps' + aIdentificador + aVersao + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');

         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(Cnpj) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

         if (Provedor <> proBetha) or (IM <> '') then
           Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
         Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

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

  if Provedor in [proNenhum] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsSitLote: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proCONAM:  begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('Sdt_consultaprotocoloin xmlns="NFe"');
                 Gerador.wGrupoNFSe('Protocolo>' + Protocolo + '</Protocolo');
                 Gerador.wGrupoNFSe('Login');
                 Gerador.wGrupoNFSe('CodigoUsuario>' + UserWeb + '</CodigoUsuario');
                 Gerador.wGrupoNFSe('CodigoContribuinte>' + SenhaWeb + '</CodigoContribuinte');
                 Gerador.wGrupoNFSe('/Login');
                 Gerador.wGrupoNFSe('/Sdt_consultaprotocoloin');
               end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('prestador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/prestador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrLoteRps', 01, 14, 1, NumeroLote, '');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#2', 'numeroProtocolo', 01, 50, 1, Protocolo, '');
             (*
             Gerador.wGrupoNFSe('IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupoNFSe('/IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'numeroProtocolo', 01, 50, 1, Protocolo, '');
             *)
           end;

    proInfisc,
    proInfiscv11: begin
                    Gerador.Prefixo := '';
                    Gerador.wCampoNFSe(tcStr, '#1', 'CNPJ', 14, 14, 1, Cnpj, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'cLote', 01, 15, 1, Protocolo, '');
                  end;

    proISSDSF: begin
                 // Não possui 
               end;

    proNFSEBrasil: begin
                     Gerador.ArquivoFormatoXML := Protocolo;
                   end;

    proSP, 
    proNotaBlu: begin
                  Gerador.wGrupoNFSe('Cabecalho' + aVersao + ' xmlns=""');
                  Gerador.wGrupoNFSe('CPFCNPJRemetente');
                  Gerador.wCampoCNPJCPF('', '', Cnpj);
                  Gerador.wGrupoNFSe('/CPFCNPJRemetente');
                  Gerador.wCampoNFSe(tcStr, '#3', 'NumeroLote', 01, 14, 1, NumeroLote, '');
                  Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoPrestador', 01, 15, 1, IM, '');
                  Gerador.wGrupoNFSe('/Cabecalho');
                end;

  else begin
         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('Prestador' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(Cnpj) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

         if (Provedor <> proBetha) or (IM <> '') then
           Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/Prestador');

         Gerador.wCampoNFSe(tcStr, '#4', 'Protocolo', 01, 50, 1, Protocolo, '', True, aNameSpace);
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, pro4R, proAgili,
                  proCoplan, profintelISS, proFiorilli, proFriburgo, proGoiania,
                  proGovDigital, proISSDigital, proISSe, proProdata, proVirtual,
                  proSaatri, proFreire, proPVH, proVitoria, proTecnos, proSiam,
                  proSisPMJP, proSystemPro] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsLote: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proAgili,
    proAgiliv2: begin
                  Gerador.wGrupoNFSe('IdentificacaoPrestador');
                  Gerador.wCampoNFSe(tcStr, '', 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');
                  Gerador.wGrupoNFSe('CpfCnpj');

                  if Length(Cnpj) <= 11 then
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
                  else
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

                  Gerador.wGrupoNFSe('/CpfCnpj');
                  Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                  Gerador.wGrupoNFSe('/IdentificacaoPrestador');
                  Gerador.wCampoNFSe(tcStr, '#4', 'Protocolo', 01, 50, 1, Protocolo, '', True, aNameSpace);

                  if VersaoNFSe = ve100 then
                    Gerador.wCampoNFSe(tcStr, '', 'Versao', 4, 4, 1, '1.00', '');
                end;

    proCONAM: begin
                Gerador.Prefixo := '';
                Gerador.wGrupoNFSe('Sdt_consultanotasprotocoloin xmlns="NFe"');
                Gerador.wGrupoNFSe('Protocolo>' + Protocolo + '</Protocolo');
                Gerador.wGrupoNFSe('Login');
                Gerador.wGrupoNFSe('CodigoUsuario>' + UserWeb + '</CodigoUsuario');
                Gerador.wGrupoNFSe('CodigoContribuinte>' + SenhaWeb + '</CodigoContribuinte');
                Gerador.wGrupoNFSe('/Login');
                Gerador.wGrupoNFSe('/Sdt_consultanotasprotocoloin');
              end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('prestador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/prestador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrLoteRps', 01, 14, 1, NumeroLote, '');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#2', 'numeroProtocolo', 01, 50, 1, Protocolo, '');
             (*
             Gerador.wGrupoNFSe('IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupoNFSe('/IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'numeroProtocolo', 01, 50, 1, Protocolo, '');
             *)
           end;

    proInfisc,
    proInfiscv11: begin
                    // Não Possui
                  end;

    proISSDSF,
    proCTA: begin
              Gerador.Prefixo := '';
              Gerador.wGrupoNFSe('Cabecalho');
              if Provedor = proCTA then
                Gerador.wCampoNFSe(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
              if Provedor = proCTA then  // provedor bugado, pede o NumeroLote mas na verdade exige o Protocolo
                Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 14, 1, Protocolo, '')
              else
                Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 14, 1, NumeroLote, '');
              Gerador.wGrupoNFSe('/Cabecalho');
            end;

    proNFSEBrasil: begin
                     Gerador.ArquivoFormatoXML := Protocolo;
                   end;

    proSP,
    proNotaBlu: begin
                  Gerador.wGrupoNFSe('Cabecalho' + aVersao + ' xmlns=""');
                  Gerador.wGrupoNFSe('CPFCNPJRemetente');
                  Gerador.wCampoCNPJCPF('', '', Cnpj);
                  Gerador.wGrupoNFSe('/CPFCNPJRemetente');
//                Gerador.wCampoNFSe(tcStr, '#2', 'CNPJRemetente', 14, 14, 1, Cnpj, '');
                  Gerador.wCampoNFSe(tcStr, '#3', 'NumeroLote', 01, 14, 1, NumeroLote, '');
                  Gerador.wGrupoNFSe('/Cabecalho');
                end;

    proSMARAPD: begin
                  Gerador.ArquivoFormatoXML := '<recibo><codrecibo>'+ Protocolo +'</codrecibo></recibo>';
               end;

    proIPM: begin
              Gerador.wGrupoNFSe('nfse');
              Gerador.wGrupoNFSe('pesquisa');
              Gerador.wCampoNFSe(tcStr, '', 'codigo_autenticidade', 01, 16, 0, Protocolo, '');
              Gerador.wCampoNFSe(tcStr, '', 'numero', 0, 9, 1, '', '');
              Gerador.wCampoNFSe(tcStr, '', 'serie', 0, 1, 1, '', '');
              Gerador.wCampoNFSe(tcStr, '', 'cadastro', 0, 9, 1, '', '');
              Gerador.wGrupoNFSe('/pesquisa');
              Gerador.wGrupoNFSe('/nfse');
            end;
  else begin
         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('Prestador' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(Cnpj) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

         if Provedor = proTecnos then
           Gerador.wCampoNFSe(tcStr, '#3', 'RazaoSocial', 01, 115, 1, RazaoSocial, '');

         if Provedor = proNFSeBrasil then
           Gerador.wCampoNFSe(tcStr, '#3', 'codMunicipio', 01, 07, 1, IntToStr(CodMunicipio), '');

         if (Provedor <> proBetha) or (IM <> '') then
           Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

         if Provedor = proISSDigital then
         begin
           Gerador.wCampoNFSe(tcStr, '#5', 'Senha', 06, 10, 1, Senha, '');
           Gerador.wCampoNFSe(tcStr, '#6', 'FraseSecreta', 06, 20, 1, FraseSecreta, '');
         end;

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/Prestador');

         Gerador.wCampoNFSe(tcStr, '#4', 'Protocolo', 01, 50, 1, Protocolo, '', True, aNameSpace);
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsNFSeRPS: String;
var
  ok: Boolean;
  _TipoRps: string;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proAgili,
    proAgiliv2: begin
                  if VersaoNFSe = ve100 then
                    Gerador.wCampoNFSe(tcStr, '#1', 'UnidadeGestora', 14, 14, 1, CNPJPrefeitura, '');

                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupoNFSe('IdentificacaoRps' + aNameSpace);

                  Gerador.Prefixo := Prefixo4;
                  Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, NumeroRps, '');
                  Gerador.wCampoNFSe(tcStr, '#2', 'Serie', 01, 05, 1, SerieRps, '');

                  case StrToTipoRPS(ok, TipoRps) of
                    trRPS: _TipoRps := '-2';
                    trNFConjugada: _TipoRps := '-4';
                    trCupom: _TipoRps := '-5';
                  end;

                  Gerador.wCampoNFSe(tcStr, '#3', 'Tipo', 01, 01, 1, _TipoRps, '');

                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupoNFSe('/IdentificacaoRps');

                  Gerador.wGrupoNFSe('IdentificacaoPrestador');
                  Gerador.wCampoNFSe(tcStr, '' , 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('CpfCnpj');

                  if Length(Cnpj) <= 11 then
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
                  else
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

                  Gerador.wGrupoNFSe('/CpfCnpj');
                  Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupoNFSe('/IdentificacaoPrestador');

                  if VersaoNFSe = ve100 then
                    Gerador.wCampoNFSe(tcStr, '#5', 'Versao', 4, 4, 1, '1.00', '');
                end;

    proInfisc,
    proInfiscv11: begin
                    // Não Possui
                  end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('rps');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrRps', 01, 15, 1, NumeroRps, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrEmissorRps', 01, 01, 1, '1', '');
                    Gerador.wGrupoNFSe('/rps');
                    Gerador.wGrupoNFSe('prestador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/prestador');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#2', 'identificacaoRps', 01, 15, 1, NumeroRps, '');
             (*
             Gerador.wGrupoNFSe('IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupoNFSe('/IdentificacaoPrestador');
             Gerador.wGrupoNFSe('IdentificacaoRps');
             Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, NumeroRps, '');
             Gerador.wCampoNFSe(tcStr, '#2', 'Serie', 01, 05, 1, SerieRps, '');
             Gerador.wCampoNFSe(tcStr, '#3', 'Tipo', 01, 01, 1, TipoRps, '');
             Gerador.wGrupoNFSe('/IdentificacaoRps');
             *)
           end;

    proISSDSF,
    proCTA: begin
              Gerador.Prefixo := '';
              Gerador.wGrupoNFSe('Cabecalho');
              if Provedor = proCTA then
                Gerador.wCampoNFSe(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'transacao', 01, 14, 1, LowerCase(booltostr(Transacao, True)), '');
              Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
              Gerador.wGrupoNFSe('/Cabecalho');

              Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<Lote' + aIdentificador + '>' +
                                            Notas +
                                          '</Lote>';
            end;

    proGoverna: begin
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('ConsultaRps');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wCampoNFSe(tcStr, '', 'CodCadBic', 01, 10, 1, IM, '');

                  if CodMunicipio = 3104007  then //Araxá
                    Gerador.wCampoNFSe(tcStr, '', 'VrsArq', 01, 01, 1, '4', '')
                  else
                    Gerador.wCampoNFSe(tcStr, '', 'VrsArq', 01, 10, 1, '1', '');

                  Gerador.wCampoNFSe(tcStr, '', 'ChvAcs', 01, 30, 1, ChaveAcessoPrefeitura, '');
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('InfConsultaRPS');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wCampoNFSe(tcStr, '', 'NumRPS', 01, 10, 1, NumeroRps, '');
                  Gerador.wCampoNFSe(tcStr, '', 'CodVer', 01, 10, 1, CodVerificacaoRPS, '');
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('/InfConsultaRPS');
                  Gerador.wGrupoNFSe('/ConsultaRps');
                end;

    proNFSEBrasil: begin
                     // Nenhum valor
                   end;

    proSP, 
    proNotaBlu: begin
                  Gerador.wGrupoNFSe('Cabecalho' + aVersao + ' xmlns=""');
                  Gerador.wGrupoNFSe('CPFCNPJRemetente');
                  Gerador.wCampoCNPJCPF('', '', Cnpj);
                  Gerador.wGrupoNFSe('/CPFCNPJRemetente');
//                  Gerador.wCampoNFSe(tcStr, '#2', 'CNPJRemetente', 14, 14, 1, Cnpj, '');
                  Gerador.wGrupoNFSe('/Cabecalho');
                  Gerador.wGrupoNFSe('Detalhe');
                  Gerador.wGrupoNFSe('ChaveRPS');
                  Gerador.wCampoNFSe(tcStr, '', 'InscricaoPrestador', 01, 11, 1, IM, '');
                  Gerador.wCampoNFSe(tcStr, '', 'SerieRPS', 01, 02, 1, SerieRPS, '');
                  Gerador.wCampoNFSe(tcStr, '', 'NumeroRPS', 01, 12, 1, NumeroRPS, '');
                  Gerador.wGrupoNFSe('/ChaveRPS');
                  Gerador.wGrupoNFSe('/Detalhe');
                end;
  else begin
         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('IdentificacaoRps' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, NumeroRps, '');
         Gerador.wCampoNFSe(tcStr, '#2', 'Serie', 01, 05, 1, SerieRps, '');
         Gerador.wCampoNFSe(tcStr, '#3', 'Tipo', 01, 01, 1, TipoRps, '');

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/IdentificacaoRps');

         Gerador.wGrupoNFSe('Prestador' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon, pro4R]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(Cnpj) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

         if Provedor = proTecnos then
           Gerador.wCampoNFSe(tcStr, '#3', 'RazaoSocial', 01, 115, 1, RazaoSocial, '');

         if (Provedor <> proBetha) or (IM <> '') then
           Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

         if Provedor = proISSDigital then
         begin
           Gerador.wCampoNFSe(tcStr, '#5', 'Senha', 06, 10, 1, Senha, '');
           Gerador.wCampoNFSe(tcStr, '#6', 'FraseSecreta', 06, 20, 1, FraseSecreta, '');
         end;

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/Prestador');
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proSiam] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsNFSe: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proAgili,
    proAgiliv2: begin
                  Gerador.Prefixo := '';
                  if VersaoNFSe = ve100 then
                    Gerador.wCampoNFSe(tcStr, '', 'UnidadeGestora', 14, 14, 1, CNPJPrefeitura, '');

                  Gerador.wGrupoNFSe('IdentificacaoPrestador');
                  Gerador.wCampoNFSe(tcStr, '', 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');
                  Gerador.wGrupoNFSe('CpfCnpj');

                  if Length(Cnpj) <= 11 then
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
                  else
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

                  Gerador.wGrupoNFSe('/CpfCnpj');
                  Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                  Gerador.wGrupoNFSe('/IdentificacaoPrestador');
                  Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNfseInicial', 01, 15, 1, NumeroNFSe, '');
                  Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNfseFinal', 01, 15, 1, NumeroNFSe, '');

                  if VersaoNFSe = ve100 then
                    Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 04, 04, 1, '1.00', '');
                end;
              
    proInfisc,
    proInfiscv11: begin
                    Gerador.Prefixo := '';
                    Gerador.wCampoNFSe(tcStr, '#1', 'CNPJ', 14, 14, 1, Cnpj, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'notaInicial', 01, 15, 1, NumeroNFSe, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'notaFinal', 01, 15, 1, NumeroNFSe, '');
                    Gerador.wCampoNFSe(tcDat, '#1', 'emissaoInicial', 01, 15, 1, DataInicial, '');
                    Gerador.wCampoNFSe(tcDat, '#1', 'emissaoFinal', 01, 15, 1, DataFinal, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'serieNotaFiscal', 01, 15, 1, SerieNFSe, '');
                  end;

    proEquiplano: begin
                    // Nao Possui
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#2', 'numeroNfse', 01, 15, 1, NumeroNFSe, '');
             Gerador.wCampoNFSe(tcDat, '#3', 'dataInicial', 10, 10, 1, DataInicial, '');
             Gerador.wCampoNFSe(tcDat, '#4', 'dataFinal', 10, 10, 1, DataFinal, '');
             Gerador.wCampoNFSe(tcStr, '#5', 'identificacaoTomador', 11, 14, 1, CNPJTomador, '');
             Gerador.wCampoNFSe(tcStr, '#6', 'identificacaoItermediarioServico', 11, 14, 1, CNPJInter, '');

             (*
             Gerador.wGrupoNFSe('IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupoNFSe('/IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'numeroNfse', 01, 15, 1, NumeroNFSe, '');
             Gerador.wCampoNFSe(tcDat, '#1', 'dataInicial', 10, 10, 1, DataInicial, '');
             Gerador.wCampoNFSe(tcDat, '#1', 'dataFinal', 10, 10, 1, DataFinal, '');
             Gerador.wGrupoNFSe('IdentificacaoTomador');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJTomador, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJTomador)<>14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IMTomador, '');
             Gerador.wGrupoNFSe('/IdentificacaoTomador');
             Gerador.wGrupoNFSe('IdentificacaoIntermediario');
             Gerador.wCampoNFSe(tcStr, '#1', 'RazaoSocial', 01, 120, 1, NomeInter, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJInter, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJInter)<>14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IMInter, '');
             Gerador.wGrupoNFSe('/IdentificacaoIntermediario');
             *)
           end;

    proISSDSF,
    proCTA: begin
              Gerador.Prefixo := '';
              Gerador.wGrupoNFSe('Cabecalho');
              if Provedor = proCTA then
                Gerador.wCampoNFSe(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipalPrestador', 01, 15, 1, IM, '');
              Gerador.wCampoNFSe(tcDat, '#1', 'dtInicio', 10, 10, 1, DataInicial, '');
              Gerador.wCampoNFSe(tcDat, '#1', 'dtFim', 10, 10, 1, DataFinal, '');
              if Provedor = proISSDSF then
                Gerador.wCampoNFSe(tcStr, '#1', 'NotaInicial', 01, 15, 1, NumeroNFSe, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
              Gerador.wGrupoNFSe('/Cabecalho');
            end;

    proSP, 
    proNotaBlu: begin
             Gerador.wGrupoNFSe('Cabecalho' + aVersao + ' xmlns=""');
             Gerador.wGrupoNFSe('CPFCNPJRemetente');
             Gerador.wCampoCNPJCPF('', '', Cnpj);
             Gerador.wGrupoNFSe('/CPFCNPJRemetente');
//             Gerador.wCampoNFSe(tcStr, '#2', 'CNPJRemetente', 14, 14, 1, Cnpj, '');
             Gerador.wGrupoNFSe('/Cabecalho');
             Gerador.wGrupoNFSe('Detalhe');
             Gerador.wGrupoNFSe('ChaveNFe');
             Gerador.wCampoNFSe(tcStr, '', 'InscricaoPrestador', 01, 11, 1, IM, '');
             Gerador.wCampoNFSe(tcStr, '', 'Numero', 01, 12, 1, NumeroNFSe, '');
//             Gerador.wCampoNFSe(tcStr, '', 'CodigoVerificacao', 01, 8, 0, CodVerificacaoRPS, '');
             Gerador.wGrupoNFSe('/ChaveNFe');
             Gerador.wGrupoNFSe('/Detalhe');
           end;
    proGoverna :
              begin
               Gerador.Prefixo := Prefixo3;
               Gerador.wCampoNFSe(tcStr, '#1', 'CodCadBic', 01, 15, 1, IMTomador, '');
               Gerador.wCampoNFSe(tcStr, '#1', 'ChvAcs', 01, 15, 1, SerieNFSe, '');
               Gerador.wCampoNFSe(tcStr, '#1', 'NumNot', 01, 15, 1, NumeroNFSe, '');
               Gerador.wCampoNFSe(tcStr, '#1', 'CodVer', 01, 15, 1, NomeInter, '');
              end;
  else begin
         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('Prestador' + aNameSpace);

         Gerador.Prefixo := Prefixo4;
         if (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(Cnpj) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

//         if Provedor = proTecnos then
//           Gerador.wCampoNFSe(tcStr, '#3', 'RazaoSocial', 01, 115, 1, RazaoSocial, '');

         if (Provedor <> proBetha) or (IM <> '') then
           Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

         if Provedor = proISSDigital then
         begin
           Gerador.wCampoNFSe(tcStr, '#5', 'Senha', 06, 10, 1, Senha, '');
           Gerador.wCampoNFSe(tcStr, '#6', 'FraseSecreta', 06, 20, 1, FraseSecreta, '');
         end;

         Gerador.Prefixo := Prefixo3;
         Gerador.wGrupoNFSe('/Prestador');

         if NumeroNFSe <> '' then
         begin
           if Provedor in [proPublica, proPVH, proSisPMJP, proSystemPro, proTecnos] then
           begin
             Gerador.wGrupoNFSe('Faixa');
             Gerador.wCampoNFSe(tcStr, '#5', 'NumeroNfseInicial', 01, 15, 1, NumeroNFSe, '');
             Gerador.wCampoNFSe(tcStr, '#6', 'NumeroNfseFinal', 01, 15, 1, NumeroNFSe, '');
             Gerador.wGrupoNFSe('/Faixa');
           end
           else
             Gerador.wCampoNFSe(tcStr, '#5', 'NumeroNfse', 01, 15, 1, NumeroNFSe, '', True, aNameSpace);
         end;

         if ((DataInicial>0) and (DataFinal>0)) and (provedor <> proPVH) then
         begin
           Gerador.wGrupoNFSe('PeriodoEmissao' + aNameSpace);
           Gerador.wCampoNFSe(tcDat, '#5', 'DataInicial', 10, 10, 1, DataInicial, '');
           Gerador.wCampoNFSe(tcDat, '#6', 'DataFinal', 10, 10, 1, DataFinal, '');
           Gerador.wGrupoNFSe('/PeriodoEmissao');
         end;

         if (CNPJTomador <> '') or (IMTomador <> '')then
         begin
           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('Tomador' + aNameSpace);

           Gerador.Prefixo := Prefixo4;
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(CnpjTomador) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, CnpjTomador, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, CnpjTomador, '');
           Gerador.wGrupoNFSe('/CpfCnpj');

           Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IMTomador, '');

           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('/Tomador');
         (*
         end
         else begin
           if Provedor = proTecnos then
           begin
             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupoNFSe('Tomador' + aNameSpace);
             Gerador.Prefixo := Prefixo4;
             Gerador.wGrupoNFSe('CpfCnpj');
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, '', '');
             Gerador.wGrupoNFSe('/CpfCnpj');
             Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, '', '');
             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupoNFSe('/Tomador');
           end;
         *)
         end;

         if (NomeInter <> '') and (CNPJInter <> '') then
         begin
           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('IntermediarioServico' + aNameSpace);

           Gerador.Prefixo := Prefixo4;
           Gerador.wCampoNFSe(tcStr, '#4', 'RazaoSocial', 01, 115, 1, NomeInter, '');

           if (VersaoNFSe <> ve100) or (Provedor in [proActcon, proISSNet]) then
           begin
             Gerador.wGrupoNFSe('CpfCnpj');
             if Length(CnpjInter) <= 11 then
               Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, CnpjInter, '')
             else
               Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, CnpjInter, '');
             Gerador.wGrupoNFSe('/CpfCnpj');
           end
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, CnpjInter, '');

           Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IMInter, '');

           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupoNFSe('/IntermediarioServico');
         (*
         end
         else begin
           if Provedor = proTecnos then
           begin
             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupoNFSe('IntermediarioServico' + aNameSpace);
             Gerador.Prefixo := Prefixo4;
             Gerador.wCampoNFSe(tcStr, '#4', 'RazaoSocial', 01, 115, 1, '', '');
             Gerador.wGrupoNFSe('CpfCnpj');
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, '', '');
             Gerador.wGrupoNFSe('/CpfCnpj');
             Gerador.wCampoNFSe(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, '', '');
             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupoNFSe('/IntermediarioServico');
           end;
         *)
         end;

         if Provedor in [proDigifred, profintelISS, proFiorilli, proPronimv2,
                         proPVH, proSisPMJP, proSystemPro, proTecnos] then
           Gerador.wCampoNFSe(tcInt, '#4', 'Pagina', 01, 06, 1, Pagina, '');
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proSiam] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgCancelarNFSe: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proAgili,
    proAgiliv2: begin
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('IdentificacaoNfse');
                  Gerador.wCampoNFSe(tcStr, '#3', 'Numero', 01, 15, 1, NumeroNfse, '');

                  if VersaoNFSe = ve100 then
                  begin
                    Gerador.wGrupoNFSe('IdentificacaoPrestador');
                    Gerador.wCampoNFSe(tcStr, '', 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');
                  end;

                  Gerador.wGrupoNFSe('CpfCnpj');
                  if Length(Cnpj) <= 11 then
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
                  else
                    Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
                  Gerador.wGrupoNFSe('/CpfCnpj');

                  Gerador.wCampoNFSe(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');

                  if VersaoNFSe = ve100 then
                    Gerador.wGrupoNFSe('/IdentificacaoPrestador');

                  Gerador.wGrupoNFSe('/IdentificacaoNfse');

                  Gerador.wCampoNFSe(tcStr, '#1', 'CodigoCancelamento', 01, 01, 1, CodigoCanc, '');

                  if VersaoNFSe = ve100 then
                  begin
                    Gerador.wCampoNFSe(tcStr, '#1', 'JustificativaCancelamento', 01, 255, 1, MotivoCanc, '');
                    Gerador.wCampoNFSe(tcStr, '', 'Versao', 4, 4, 1, '1.00', '');
                  end;
                end;

    proCONAM: begin
                Gerador.Prefixo := '';
                Gerador.Opcoes.DecimalChar := '.';
                Gerador.wGrupoNFSe('Sdt_cancelanfe xmlns="NFe"');
                Gerador.wGrupoNFSe('Login');
                Gerador.wGrupoNFSe('CodigoUsuario>' + UserWeb + '</CodigoUsuario');
                Gerador.wGrupoNFSe('CodigoContribuinte>' + SenhaWeb + '</CodigoContribuinte');
                Gerador.wGrupoNFSe('/Login');
                Gerador.wGrupoNFSe('Nota');
                Gerador.wGrupoNFSe('SerieNota>' + SerieNFSe + '</SerieNota');
                Gerador.wGrupoNFSe('NumeroNota>' + NumeroNfse + '</NumeroNota');
                Gerador.wGrupoNFSe('SerieRPS>' + SerieRps + '</SerieRPS');
                Gerador.wGrupoNFSe('NumeroRps>' + NumeroRps + '</NumeroRps');
                Gerador.wGrupoNFSe('ValorNota>' + FloatToString(ValorNota) + '</ValorNota');
                Gerador.wGrupoNFSe('MotivoCancelamento>' + MotivoCanc + '</MotivoCancelamento');
                Gerador.wGrupoNFSe('PodeCancelarGuia>S</PodeCancelarGuia');
                Gerador.wGrupoNFSe('/Nota');
                Gerador.wGrupoNFSe('/Sdt_cancelanfe');
              end;

    proEGoverneISS: begin
                      Gerador.Prefixo := 'rgm:';
                      Gerador.wCampoNFSe(tcStr, '#1', 'ChaveAutenticacao', 01, 36, 1, ChaveAcessoPrefeitura, '');
                      Gerador.wCampoNFSe(tcStr, '#1', 'Homologacao', 04, 05, 1, LowerCase(booltostr(Transacao, True)), '');
                      Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNota', 01, 15, 1, NumeroNFSe, '');
                    end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupoNFSe('prestador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupoNFSe('/prestador');
                    Gerador.wCampoNFSe(tcStr, '#1', 'nrNfse', 01, 15, 1, NumeroNFSe, '');
                    Gerador.wCampoNFSe(tcStr, '#1', 'dsMotivoCancelamento', 01, 255, 1, MotivoCanc, '');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#2', 'numeroNfse', 01, 15, 1, NumeroNFSe, '');

             (*
             Gerador.wGrupoNFSe('IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampoNFSe(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampoNFSe(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupoNFSe('/IdentificacaoPrestador');
             Gerador.wCampoNFSe(tcStr, '#1', 'numeroNfse', 01, 15, 1, NumeroNFSe, '');
             *)
           end;

    proGinfes: begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('Prestador');

                 Gerador.Prefixo := Prefixo4;
                 Gerador.wCampoNFSe(tcStr, '#1', 'Cnpj', 14, 14, 1, Cnpj, '');
                 Gerador.wCampoNFSe(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');

                 Gerador.Prefixo := '';
                 Gerador.wGrupoNFSe('/Prestador');
                 Gerador.wCampoNFSe(tcStr, '#3', 'NumeroNfse', 01, 15, 1, NumeroNfse, '');
               end;

    proGoverna: begin
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('LoteCancelamento');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wCampoNFSe(tcStr, '', 'CodCadBic', 01, 15, 1, IM, '');

                  if CodMunicipio = 3104007  then //Araxá
                    Gerador.wCampoNFSe(tcStr, '', 'VrsArq', 01, 01, 1, '4', '')
                  else
                    Gerador.wCampoNFSe(tcStr, '', 'VrsArq', 01, 01, 1, '1', '');
                    
                  Gerador.wCampoNFSe(tcStr, '', 'ChvAcs', 30, 30, 1, ChaveAcessoPrefeitura, '');
                  Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupoNFSe('/LoteCancelamento');
                end;

    proInfisc,
    proInfiscv11: begin
                    Gerador.Prefixo := '';
                    Gerador.wCampoNFSe(tcStr, '#1', 'CNPJ', 14, 14, 1, Cnpj, '');
                    Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                  end;

    proISSDSF,
    proCTA: begin
              Gerador.Prefixo := '';
              Gerador.wGrupoNFSe('Cabecalho');
              if Provedor = proCTA then
                Gerador.wCampoNFSe(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
              Gerador.wCampoNFSe(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
              Gerador.wCampoNFSe(tcStr, '#1', 'transacao', 01, 05, 1, LowerCase(booltostr(Transacao, True)), '');
              Gerador.wCampoNFSe(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
              Gerador.wGrupoNFSe('/Cabecalho');

              Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<Lote' + aIdentificador + '>' +
                                            Notas +
                                          '</Lote>';
            end;

    proIssCuritiba: begin
                      Gerador.Prefixo := '';
                      Gerador.wGrupoNFSe('InfPedidoCancelamento');
                      Gerador.wGrupoNFSe('IdentificacaoNfse');
                      Gerador.wCampoNFSe(tcStr, '#1', 'Cnpj', 14, 14, 1, Cnpj, '');
                      Gerador.wCampoNFSe(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                      Gerador.wCampoNFSe(tcStr, '#3', 'Numero', 01, 15, 1, NumeroNfse, '');
                      Gerador.wGrupoNFSe('/IdentificacaoNfse');
                      Gerador.wGrupoNFSe('/InfPedidoCancelamento');
                    end;

    proNFSEBrasil: begin
                     Gerador.ArquivoFormatoXML := NumeroRps;
                   end;

    proSP,
    proNotaBlu: begin
             Gerador.wGrupoNFSe('Cabecalho' + aVersao + ' xmlns=""');
             Gerador.wGrupoNFSe('CPFCNPJRemetente');
             Gerador.wCampoCNPJCPF('', '', Cnpj);
             Gerador.wGrupoNFSe('/CPFCNPJRemetente');
             Gerador.wCampoNFSe(tcStr, '#3', 'transacao', 01, 05, 0, LowerCase(BooltoStr(Transacao, True)), '');
             Gerador.wGrupoNFSe('/Cabecalho');
             Gerador.wGrupoNFSe('Detalhe xmlns=""');
             Gerador.wGrupoNFSe('ChaveNFe');
             Gerador.wCampoNFSe(tcStr, '', 'InscricaoPrestador', 01, 11, 1, IM, '');
             Gerador.wCampoNFSe(tcStr, '', 'NumeroNFe', 01, 12, 1, NumeroNFSe, '');
             Gerador.wGrupoNFSe('/ChaveNFe');
             Gerador.wCampoNFSe(tcStr, '', 'AssinaturaCancelamento', 01, 2000, 1, AssinaturaCan, '');
             Gerador.wGrupoNFSe('/Detalhe');
           end;

    proSMARAPD: begin
                  Gerador.wGrupoNFSe('nfd');
                  Gerador.wGrupoNFSe('inscricaomunicipalemissor>' + IM + '</inscricaomunicipalemissor');
                  Gerador.wGrupoNFSe('numeronf>' + NumeroNFSe + '</numeronf');
                  Gerador.wGrupoNFSe('motivocancelamento>' + MotivoCanc + '</motivocancelamento');
                  Gerador.wGrupoNFSe('datacancelamento>' + FormatDateTime('dd/mm/yyyy', now) + '</datacancelamento');
                  Gerador.wGrupoNFSe('/nfd');
               end;
  else begin
         Gerador.Prefixo := Prefixo4;
         Gerador.wGrupoNFSe('IdentificacaoNfse');
         Gerador.wCampoNFSe(tcStr, '#3', 'Numero', 01, 15, 1, NumeroNfse, '');

         if (VersaoNFSe <> ve100) or (Provedor in [proActcon, pro4R]) then
         begin
           Gerador.wGrupoNFSe('CpfCnpj');
           if Length(Cnpj) <= 11 then
             Gerador.wCampoNFSe(tcStr, '#2', 'Cpf', 11, 11, 1, Cnpj, '')
           else
             Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
           Gerador.wGrupoNFSe('/CpfCnpj');
         end
         else
           Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

         if (Provedor <> proBetha) or (IM <> '') then
           Gerador.wCampoNFSe(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');
         Gerador.wCampoNFSe(tcInt, '#2', 'CodigoMunicipio', 01, 07, 1, CodMunicipio, '');

         Gerador.wGrupoNFSe('/IdentificacaoNfse');

         Gerador.wCampoNFSe(tcStr, '#1', 'CodigoCancelamento', 01, 01, 1, CodigoCanc, '');

         if Provedor in [proPublica, proTecnos, proFriburgo] then
           Gerador.wCampoNFSe(tcStr, '#1', 'MotivoCancelamento', 01, 255, 1, MotivoCanc, '')
         else if Provedor in [proISSNET] then
           Gerador.wCampoNFSe(tcStr, '#1', 'MotivoCancelamentoNfse', 01, 255, 1, MotivoCanc, ''); // Filipe

//         if (Provedor in [proPublica]) and (CodigoCanc = 'C999') then
//           Gerador.wCampoNFSe(tcStr, '#1', 'MotivoCancelamento', 01, 255, 1, MotivoCanc, '');

         Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                      '</' + Prefixo4 + 'InfPedidoCancelamento>';
       end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proSiam] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgGerarNFSe: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proAgili: begin
                Gerador.wCampoNFSe(tcStr, '', 'UnidadeGestora', 14, 14, 1, CNPJPrefeitura, '');
                Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
              end;

    proAgiliv2: begin
                  Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                end;

    proInfisc,
    proInfiscv11: begin
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

    proBHISS,
    proWebISS,
    proWebISSv2: begin
                   Gerador.wGrupoNFSe('LoteRps' + aIdentificador + aVersao);

                   Gerador.Prefixo := Prefixo4;
                   Gerador.wCampoNFSe(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');
                   Gerador.wCampoNFSe(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
                   Gerador.wCampoNFSe(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                   Gerador.wCampoNFSe(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

                   Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                                '<' + Prefixo4 + 'ListaRps>' +
                                                  Notas +
                                                '</' + Prefixo4 + 'ListaRps>';

                   Gerador.Prefixo := Prefixo3;
                   Gerador.wGrupoNFSe('/LoteRps');
                 end;

    proSP, 
    proNotaBlu: begin
             Gerador.wGrupoNFSe('Cabecalho' + aVersao + ' xmlns=""');
             Gerador.wGrupoNFSe('CPFCNPJRemetente');
             Gerador.wCampoCNPJCPF('', '', Cnpj);
             Gerador.wGrupoNFSe('/CPFCNPJRemetente');
             Gerador.wGrupoNFSe('/Cabecalho');

             Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
           end;

  else
    Gerador.ArquivoFormatoXML := Notas;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proAbaco, proActcon,
                  proBetha, proBetim, proDBSeller, proEquiplano,
                  proFIssLex, proGinfes, proGovBR, proInfisc, proInfiscv11,
                  proIssCuritiba, proIssDSF, proIssIntel, proIssNet, proLexsom,
                  proNatal, proNFSEBrasil, proProdemge, proPronim, proRJ,
                  proSalvador, proSiam, proSimplIss, proSpeedGov, proThema,
                  proTinus, proTiplan] then
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
var
 TagGrupo: String;
begin
  case Provedor of
    proAgili: TagGrupo := 'PedidoCancelamento';
  else
    TagGrupo := 'Pedido';
  end;

  Result := Gera_DadosMsgCancelarNFSe + '</' + Prefixo3 + TagGrupo + '>'; //+ Notas;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgAbrirSessao: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proEL: begin
             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupoNFSe('autenticarContribuinte');

             Gerador.Prefixo := Prefixo4;
             Gerador.wCampoNFSe(tcStr, '#1', 'identificacaoPrestador', 14, 14, 1, Cnpj, '');
             Gerador.wCampoNFSe(tcStr, '#2', 'senha', 01, 14, 1, SenhaWeb, '');

             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupoNFSe('/autenticarContribuinte');
           end;

  else
    Gerador.ArquivoFormatoXML := '';
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proAbaco, proActcon,
                  proBetha, proBetim, proDBSeller, proEquiplano,
                  proFIssLex, proGinfes, proGovBR, proInfisc, proInfiscv11,
                  proIssCuritiba, proIssDSF, proIssIntel, proIssNet, proLexsom,
                  proNatal, proNFSEBrasil, proProdemge, proPronim, proRJ,
                  proSalvador, proSiam, proSimplIss, proSpeedGov, proThema,
                  proTinus, proTiplan] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgFecharSessao: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proEL: begin
             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupoNFSe('finalizarSessao');

             Gerador.Prefixo := Prefixo4;
             Gerador.wCampoNFSe(tcStr, '#1', 'hashIdentificador', 01, 40, 1, HashIdent, '');

             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupoNFSe('/finalizarSessao');
           end;

  else
    Gerador.ArquivoFormatoXML := '';
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proAbaco, proActcon,
                  proBetha, proBetim, proDBSeller, proEquiplano,
                  proFIssLex, proGinfes, proGovBR, proInfisc, proInfiscv11,
                  proIssCuritiba, proIssDSF, proIssIntel, proIssNet, proLexsom,
                  proNatal, proNFSEBrasil, proProdemge, proPronim, proRJ,
                  proSalvador, proSiam, proSimplIss, proSpeedGov, proThema,
                  proTinus, proTiplan] then
    Result := '';
end;

end.
