{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit pnfsNFSeG;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnAuxiliar, pcnGerador, pcnConversao,
  pnfsNFSe, pnfsConversao;

type

  TNFSeG = class(TPersistent)
  private
    FaVersao: String;
    FaIdentificador: String;
    FaIdentificadorCanc: String;
    FaNameSpace: String;

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
    FCodMunicipioTOM: Integer;
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
    FQtdTributos: Integer;
    FValorNota: Currency;
    FAliquotaSN: Currency;
    FAliquotaISS: Currency;
    FValorIss: Currency;
    FValorIssRetido: Currency;
    FValorTotalTributos: Currency;
    FDataOptanteSimples: TDateTime;
    FExigibilidadeISS: TnfseExigibilidadeISS;
    FRegimeEspecialTributacao: TnfseRegimeEspecialTributacao;
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
    FIdCanc: String;
    FCodigoTribMun: string;
    FKey: String;
    FAuth: String;
    FRequestId: String;
    FResposta: Integer;

    // Provedor iiBrasil
//    FIntegridade: String;

    procedure SetAtributos;
    function GetIdEntidadeEquiplano(const IBGE: Integer): String;
    procedure SetCNPJ(const Value: String);
    procedure SetCNPJTomador(const Value: String);
    procedure SetCNPJInter(const Value: String);
    procedure SetCNPJPrefeitura(const Value: String);

    procedure GerarGrupoCNPJCPF(const CNPJCPF: string; Codicao: Boolean;
      GrupoCNPJ: Boolean = False);
    procedure GerarGrupoProAdm;
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
    function Gera_DadosMsgConsURLNFSe: String;

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

    property CodMunicipio: Integer    read FCodMunicipio write FCodMunicipio;
    property CodMunicipioTOM: Integer read FCodMunicipioTOM write FCodMunicipioTOM;
    property CodigoCanc: String       read FCodigoCanc   write FCodigoCanc;
    property MotivoCanc: String       read FMotivoCanc   write FMotivoCanc;

    // Layout ISSNET.
    property CodigoTribMun : string read FCodigoTribMun write FCodigoTribMun;

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
    property AliquotaSN: Currency     read FAliquotaSN     write FAliquotaSN;
    property AliquotaIss: Currency    read FAliquotaIss    write FAliquotaIss;
    property ValorIss: Currency       read FValorIss       write FValorIss;
    property ValorIssRetido: Currency read FValorIssRetido write FValorIssRetido;
    property ValorTotalTributos: Currency read FValorTotalTributos write FValorTotalTributos;
    property DataOptanteSimples: TDateTime read FDataOptanteSimples write FDataOptanteSimples;
    property ExigibilidadeISS: TnfseExigibilidadeISS read FExigibilidadeISS write FExigibilidadeISS;
    property RegimeEspecialTributacao: TnfseRegimeEspecialTributacao read FRegimeEspecialTributacao write FRegimeEspecialTributacao;

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

    // Provedor iiBrasil
//    property Integridade: String read FIntegridade write FIntegridade;

    property IdLote: String read FIdLote write FIdLote;
    property IdCanc: String read FIdCanc write FIdCanc;
    // Layout - AdmNotas
    property Key: String          read FKey        write FKey;
    property Auth: String         read FAuth       write FAuth;
    property RequestId: String    read FRequestId  write FRequestId;
    property Resposta: Integer    read FResposta   write FResposta;
   end;

implementation

uses
  StrUtils, DateUtils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrValidador;

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
  inherited Create;
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
    if Provedor in [proSP, proNotaBlu] then
      FaVersao := ' Versao="' + VersaoDados + '"'
    else
      FaVersao := ' versao="' + VersaoDados + '"';

    if (Provedor = proFintelISS) and (CodMunicipio <> 3136702) then
      FaVersao := '';

    if Provedor in [proAbaco, proBetha, proDBSeller, proGinfes, proGoiania,
                    proGovBR, proIssCuritiba, proISSNET, proLexsom, proNatal,
                    proTinus, proRecife, proRJ, proThema, proTiplan,
                    proAgiliv2, proFISSLex, proSpeedGov, proPronim, proSalvador,
                    proSJP, proWebISS, proMetropolisWeb,
                    progeNFe, proSiapSistemas] then
      FaVersao := '';
  end
  else
    FaVersao := '';

  // Atributo NameSapce ========================================================
  if Provedor in [proEL, proSimplISS, proSimplISSv2] then
    FaNameSpace := ' ' + NameSpaceDad
  else
    FaNameSpace := '';

  // Valor do atributo Id ======================================================
  case Provedor of
    proAbaco: begin
                if CodMunicipio = 1302603 then
                  IdLote := 'L' + NumeroLote
                else
                  IdLote := NumeroLote;
              end;

    proActcon,
    proBelford,
    proBethav2,
    proIssDSF,
    proSIAPNet,
    proGiss,
    proRLZ,
    proTinus,
    proSalvador,
    proSimplISSv2,
    proSmarAPDv1,
    proSiat: IdLote := 'lote' + NumeroLote;

    proSaatri: IdLote := 'Lote_' + NumeroLote + '_' + CNPJ;

    proEL: begin
             IdLote := StringOfChar('0', 15) + OnlyNumber(NumeroRps) + SerieRps;
             IdLote := copy(IdLote, length(IdLote) - 15 + 1, 15);
           end;

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

    case Provedor of
      proElotech,
      proGovBR,
      proSiapSistemas,
      proPronim: FaIdentificador := '';

//      proSimplISSv2: FaIdentificador := ' id="' + IdLote + '"';
    else
      FaIdentificador := ' ' + Identificador + '="' + IdLote + '"';
    end;
  end
  else
    FaIdentificador := '';

  // Atributo Id do Cancelamento ===============================================
  if Identificador <> '' then
  begin
    FaIdentificadorCanc := ' ' + Identificador + '="' + IdCanc + '"';

  end
  else
    FaIdentificadorCanc := '';

  // Redefine o Profixo 3 ======================================================
  if Provedor in [proBetha, proBethav2{, proSpeedGov}] then
    Prefixo3 := '';
end;

function TNFSeG.GetIdEntidadeEquiplano(const IBGE: Integer): String;
begin
  case IBGE of
    4102307: Result :=  '23'; // Balsa Nova/PR
    4104501: Result :=  '50'; // Capanema/PR
    4104451: Result :=  '51'; // Cantagalo/PR
    4104659: Result := '141'; // Carambei/PR
    4107207: Result :=  '68'; // Dois Vizinhos/PR
    4107736: Result := '140'; // Fernandes Pinheiro/PR
    4108403: Result :=  '35'; // Francisco Beltrao/PR
    4109807: Result := '332'; // Ibipora/PR
    4113304: Result :=  '53'; // Laranjeiras do Sul/PR
    4119608: Result := '104'; // Pitanga/PR
    4120606: Result :=  '28'; // Prudentopolis/PR
    4122008: Result :=  '19'; // Rio Azul/PR
    4123501: Result :=  '54'; // Santa Helena/PR
    4125209: Result := '163'; // São Jorge D Oeste/PR
    4126306: Result :=  '61'; // Senges/PR
    4127106: Result := '260'; // Telemaco Borba/PR
    4127700: Result := '136'; // Toledo/PR
  else
    Result := '';
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
  xNotas: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proAgili,
    proAgiliv2: begin
                  if VersaoNFSe = ve100 then
                    Gerador.wCampo(tcStr, '#1', 'UnidadeGestora', 14, 14, 1, CNPJPrefeitura, '');

                  Gerador.Prefixo := Prefixo3;
                  //Gerador.wGrupo('LoteRps' + aIdentificador + aVersao + aNameSpace);
                  Gerador.wGrupo('LoteRps');

                  Gerador.Prefixo := Prefixo4;
                  Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');

                  if VersaoNFSe = ve100 then
                  begin
                    Gerador.wGrupo('IdentificacaoPrestador');
                    Gerador.wCampo(tcStr, '' , 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');
                  end;

                  GerarGrupoCNPJCPF(Cnpj, True);

                  Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');

                  if VersaoNFSe = ve100 then
                    Gerador.wGrupo('/IdentificacaoPrestador');

                  Gerador.wCampo(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

                  Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                               '<' + Prefixo4 + 'ListaRps>' +
                                                 Notas +
                                               '</' + Prefixo4 + 'ListaRps>';

                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupo('/LoteRps');

                  if VersaoNFSe = ve100 then
                    Gerador.wCampo(tcStr, '#5', 'Versao', 4, 4, 1, '1.00', '');
                end;

    proCONAM: begin
                Gerador.Opcoes.DecimalChar := ',';
                Gerador.Prefixo := '';
                Gerador.wGrupo('Sdt_processarpsin xmlns="NFe"');
                Gerador.wGrupo('Login');
                Gerador.wGrupo('CodigoUsuario>' + UserWeb + '</CodigoUsuario');
                Gerador.wGrupo('CodigoContribuinte>' + SenhaWeb + '</CodigoContribuinte');
                Gerador.wGrupo('/Login');

                // Identificaçao do RPS
                Gerador.wGrupo('SDTRPS');
                Gerador.wCampo(tcStr, '', 'Ano'    , 01, 04, 0, FormatDateTime('yyyy', DataInicial) , '');
                Gerador.wCampo(tcStr, '', 'Mes'    , 01, 02, 0, FormatDateTime('mm', DataInicial) , '');
                Gerador.wCampo(tcStr, '', 'CPFCNPJ', 01, 14, 0, CNPJ , '');
                Gerador.wCampo(tcStr, '', 'DTIni'  , 01, 10, 0, FormatDateTime('dd/mm/yyyy', DataInicial) , '');
                Gerador.wCampo(tcStr, '', 'DTFin'  , 01, 10, 0, FormatDateTime('dd/mm/yyyy', DataFinal) , '');

                if OptanteSimples = snSim then
                begin
                  Gerador.wCampo(tcInt, '', 'TipoTrib'   , 01, 01, 0, 4 , '');
                  // Data de adesao ao simples nacional
                  Gerador.wCampo(tcStr, '', 'DtAdeSN'    , 01, 10, 0, FormatDateTime('dd/mm/yyyy', DataOptanteSimples) , '');
                  Gerador.wCampo(tcDe2, '', 'AlqIssSN_IP', 01, 06, 0, AliquotaSN, '');
                end
                else
                begin
                  case ExigibilidadeISS of
                    exiExigivel:
                      Gerador.wCampo(tcInt, '', 'TipoTrib', 001, 1, 0, 1 , '');

                    exiNaoIncidencia,
                    exiIsencao,
                    exiImunidade:
                      Gerador.wCampo(tcInt, '', 'TipoTrib', 001, 1, 0, 2 , '');

                    exiSuspensaDecisaoJudicial,
                    exiSuspensaProcessoAdministrativo:
                      Gerador.wCampo(tcInt, '', 'TipoTrib', 001, 1, 0, 3 , '');

                    exiExportacao:
                      Gerador.wCampo(tcInt, '', 'TipoTrib', 001, 1, 0, 5 , '');
                  end;
                end;

                Gerador.wCampo(tcStr, '', 'Versao', 001, 4, 0, '2.00' , '');

                Gerador.wGrupo('Reg20');
                Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                Gerador.wGrupo('/Reg20');

                // Inicio do rodape registro 90
                Gerador.wGrupo('Reg90');
                Gerador.wCampo(tcStr, '', 'QtdRegNormal'  , 01, 05, 1, QtdeNotas, '');
                Gerador.wCampo(tcDe2, '', 'ValorNFS'      , 01, 16, 2, ValorTotalServicos, '');
                Gerador.wCampo(tcDe2, '', 'ValorISS'      , 01, 16, 2, ValorIss, '');
                Gerador.wCampo(tcDe2, '', 'ValorDed'      , 01, 16, 2, ValorTotalDeducoes, '');
                Gerador.wCampo(tcDe2, '', 'ValorIssRetTom', 01, 16, 2, ValorIssRetido, '');
                Gerador.wCampo(tcDe2, '', 'ValorTributos' , 01, 16, 2, ValorTotalTributos, '');
                Gerador.wCampo(tcStr, '', 'QtdReg30'      , 01, 05, 1, QtdTributos, '');
                Gerador.wGrupo('/Reg90');
                // Fim do rodape registro 90

                Gerador.wGrupo('/SDTRPS');
                Gerador.wGrupo('/Sdt_processarpsin');
              end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupo('lote');
                    Gerador.wCampo(tcStr, '#1', 'nrLote', 01, 15, 1, NumeroLote, '');
                    Gerador.wCampo(tcInt, '#1', 'qtRps', 01, 14, 1, QtdeNotas, '');
                    Gerador.wCampo(tcStr, '#1', 'nrVersaoXml', 01, 05, 1, VersaoXML, '');
                    Gerador.wGrupo('prestador');
                    Gerador.wCampo(tcStr, '#1', 'nrCnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampo(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampo(tcStr, '#1', 'isOptanteSimplesNacional', 01, 14, 1, SimNaoToStr(OptanteSimples), '');
                    Gerador.wCampo(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupo('/prestador');

                    Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                                 '<listaRps>' +
                                                   Notas +
                                                 '</listaRps>';

                    Gerador.wGrupo('/lote');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wGrupo('LoteRps' + FaNameSpace);

             Gerador.wCampo(tcStr, '#1', 'Id', 13, 32, 1, IdLote, ''); // ?? Código Verificação
             Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 14, 1, NumeroLote, '');
             Gerador.wCampo(tcInt, '#1', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');
             Gerador.wGrupo('IdentificacaoPrestador');
             Gerador.wCampo(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampo(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ) <> 14, '1', '2'), '');
             Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupo('/IdentificacaoPrestador');

             Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<ListaRps>' +
                                            Notas +
                                          '</ListaRps>';

             Gerador.wGrupo('/LoteRps');
           end;

    proInfisc,
    proInfiscv11: begin
                    Gerador.Prefixo := Prefixo3;
//                    Gerador.wGrupo('envioLote versao="1.0" xmlns:ws="http://ws.pc.gif.com.br/"');
                    Gerador.wGrupo('envioLote versao="1.0"');
                    Gerador.wCampo(tcStr, '', 'CNPJ'   , 01, 14, 1, Cnpj, '');
                    Gerador.wCampo(tcStr, '', 'dhTrans', 01, 19, 1, FormatDateTime('yyyy-mm-dd hh:mm:ss', Now), '');
                    Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                    Gerador.wGrupo('/envioLote');
                  end;

    proISSDSF,
    proCTA,
    proSiat: begin 
              Gerador.Prefixo := '';
              Gerador.wGrupo('Cabecalho');
              if Provedor = proCTA then
                Gerador.wCampo(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');
              Gerador.wCampo(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
              Gerador.wCampo(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
              Gerador.wCampo(tcStr, '#1', 'RazaoSocialRemetente', 01, 14, 1, RazaoSocial, '');
              Gerador.wCampo(tcStr, '#1', 'transacao', 01, 14, 1, LowerCase(booltostr(Transacao, True)), '');
              Gerador.wCampo(tcDat, '#1', 'dtInicio', 01, 10, 1, DataInicial, '');
              Gerador.wCampo(tcDat, '#1', 'dtFim', 01, 10, 1, DataFinal, '');
              Gerador.wCampo(tcInt, '#1', 'QtdRPS', 01, 14, 1, QtdeNotas, '');
              Gerador.wCampo(tcDe2, '#1', 'ValorTotalServicos', 01, 14, 1, ValorTotalServicos, '');
              Gerador.wCampo(tcDe2, '#1', 'ValorTotalDeducoes', 01, 14, 1, ValorTotalDeducoes, '');
              Gerador.wCampo(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
              Gerador.wCampo(tcStr, '#1', 'MetodoEnvio', 01, 02, 1, 'WS', '');
              Gerador.wGrupo('/Cabecalho');

              Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<Lote' + FaIdentificador + '>' +
                                            Notas +
                                          '</Lote>';
            end;

    proNFSEBrasil: begin
                     Atributo_cMun := ' codMunicipio="' + IntToStr(CodMunicipio) + '"';

                     Gerador.Prefixo := Prefixo3;
                     Gerador.wGrupo('LoteRps' + Atributo_cMun + FaVersao + FaIdentificador);

                     Gerador.Prefixo := Prefixo4;
                     Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');

                     Gerador.wCampo(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');

                     Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                     Gerador.wCampo(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

                     Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                                  '<' + Prefixo4 + 'ListaRps>' +
                                                    Notas +
                                                  '</' + Prefixo4 + 'ListaRps>';

                     Gerador.Prefixo := Prefixo3;
                     Gerador.wGrupo('/LoteRps');
                   end;

    proGoverna: begin
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupo('LoteRps');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wCampo(tcStr, '', 'CodCadBic', 01, 15, 1, IM, '');


                  if CodMunicipio = 3104007  then //Araxá
                    Gerador.wCampo(tcStr, '', 'VrsArq', 01, 01, 1, '4', '')
                  else
                    Gerador.wCampo(tcStr, '', 'VrsArq', 01, 01, 1, '1', '');

                  Gerador.wCampo(tcStr, '', 'ChvAcs', 30, 30, 1, ChaveAcessoPrefeitura, '');
                  Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupo('/LoteRps');
                end;

    proNotaBlu,
    proSP: begin
             Gerador.Opcoes.SuprimirDecimais := True;

             Gerador.wGrupo('Cabecalho' + FaVersao + ' xmlns=""');
             Gerador.wGrupo('CPFCNPJRemetente');
             Gerador.wCampoCNPJCPF('', '', Cnpj);
             Gerador.wGrupo('/CPFCNPJRemetente');

             Gerador.wCampo(tcStr, '#1', 'transacao', 01, 05, 0, LowerCase(booltostr(Transacao, True)), '');
             Gerador.wCampo(tcDat, '#1', 'dtInicio', 01, 10, 1, DataInicial, '');
             Gerador.wCampo(tcDat, '#1', 'dtFim', 01, 10, 1, DataFinal, '');
             Gerador.wCampo(tcInt, '#1', 'QtdRPS', 01, 14, 1, QtdeNotas, '');
             Gerador.wCampo(tcDe2, '#1', 'ValorTotalServicos', 1, 15, 1, ValorTotalServicos, '');
             Gerador.wCampo(tcDe2, '#1', 'ValorTotalDeducoes', 1, 15, 1, ValorTotalDeducoes, '');
             Gerador.wGrupo('/Cabecalho');

             Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;

             Gerador.Opcoes.SuprimirDecimais := False;
           end;

     proSMARAPD,
     proGiap,
     proIPM:
          begin
            Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas
          end;

     proISSJoinville,
     proAbacov2:
          begin
            Gerador.wGrupo('LoteRps' + FaVersao + FaIdentificador);
            Gerador.Prefixo := Prefixo4;
            Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');
            Gerador.wGrupo('Prestador');

            GerarGrupoCNPJCPF(Cnpj, True);

            Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 0, IM, '');

            Gerador.wGrupo('/Prestador');
            Gerador.wCampo(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');
            Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                       '<' + Prefixo4 + 'ListaRps>' +
                                         Notas +
                                       '</' + Prefixo4 + 'ListaRps>';
            Gerador.Prefixo := Prefixo3;
            Gerador.wGrupo('/LoteRps');
          end;

     proELv2,
     proGiss,
     proTcheInfov2,
     proActconv204,
     proSmarAPDABRASF:
          begin
           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupo('LoteRps' + FaIdentificador + FaVersao + FaNameSpace);
           Gerador.Prefixo := Prefixo4;
           Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');
           Gerador.wGrupo('Prestador');

            GerarGrupoCNPJCPF(Cnpj, True);

           Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
           Gerador.wGrupo('/Prestador');
           Gerador.wCampo(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

           Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                        '<' + Prefixo4 + 'ListaRps>' +
                                          Notas +
                                        '</' + Prefixo4 + 'ListaRps>';

           Gerador.Prefixo := Prefixo3;
           Gerador.wGrupo('/LoteRps');
          end;

     proAssessorPublico:
       begin
         xNotas := '<NFSE>';
         xNotas := xNotas + '<IDENTIFICACAO>';
         xNotas := xNotas + '<MESCOMP>' + FormatDateTime('MM', Now)+'</MESCOMP>';
         xNotas := xNotas + '<ANOCOMP>' + FormatDateTime('yyyy', Now)+'</ANOCOMP>';
         xNotas := xNotas + '<INSCRICAO>' + IM + '</INSCRICAO>';
         xNotas := xNotas + '<VERSAO>' + VersaoXML + '</VERSAO>';
         xNotas := xNotas + '</IDENTIFICACAO>';
         xNotas := xNotas + '<NOTAS>';
         xNotas := xNotas + Notas;
         xNotas := xNotas + '</NOTAS>';
         xNotas := xNotas + '</NFSE>';

         Gerador.wGrupo('Nfse.Execute xmlns="nfse"');
         Gerador.wCampo(tcStr, '', 'Operacao', 1, 1, 1, '1', '');
         Gerador.wCampo(tcStr, '', 'Usuario', 1, 1, 1, UserWeb, '');
         Gerador.wCampo(tcStr, '', 'Senha', 1, 1, 1, SenhaWeb, '');
         Gerador.wCampo(tcStr, '', 'Webxml', 1, 1, 1, xNotas, '');
         Gerador.wGrupo('/Nfse.Execute');
       end;

     proGeisWeb:
       begin
         Gerador.wCampo(tcStr, '#1', 'CnpjCpf', 01, 15, 1, CNPJ, '');
         Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');

         Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
       end;
  else
    begin
      if Provedor = proSigep then
      begin
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('credenciais');
        Gerador.wCampo(tcStr, '#01', 'usuario     ', 01, 15, 1, UserWeb);
        Gerador.wCampo(tcStr, '#02', 'senha       ', 01, 05, 1, SenhaWeb);
        Gerador.wCampo(tcStr, '#03', 'chavePrivada', 01, 01, 1, ChaveAcessoPrefeitura);
        Gerador.wGrupo('/credenciais');
      end;

      if Provedor = proElotech then
      begin
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('IdentificacaoRequerente');

        GerarGrupoCNPJCPF(Cnpj, True, False);

        Gerador.wCampo(tcStr, '#01', 'InscricaoMunicipal', 01, 15, 1, IM);
        Gerador.wCampo(tcStr, '#02', 'Senha  ', 01, 05, 1, SenhaWeb);
        Gerador.wCampo(tcStr, '#03', 'Homologa', 01, 01, 5, LowerCase(booltostr(Transacao, True)));
        Gerador.wGrupo('/IdentificacaoRequerente');
      end;

      Gerador.Prefixo := Prefixo3;

      if Provedor in [proCoplan, proSIAPNet] then
        Gerador.wGrupo('LoteRps' + FaVersao + FaIdentificador)
      else
        Gerador.wGrupo('LoteRps' + FaIdentificador + FaVersao + FaNameSpace);

      Gerador.Prefixo := Prefixo4;
      Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');

      GerarGrupoCNPJCPF(Cnpj,
                        (VersaoNFSe <> ve100) or (Provedor in [proISSNet]),
                        (Provedor = proSiapSistemas));

      if (Provedor <> proBetha) or (IM <> '') then
        Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');

      if Provedor = proAdm then
      begin
        GerarGrupoProAdm;

        Gerador.wCampo(tcInt, '#7', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');
        Gerador.wCampo(tcInt, '#5', 'Resposta', 01, 01, 1, Resposta, '');
      end
      else
        Gerador.wCampo(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                   '<' + Prefixo4 + 'ListaRps>' +
                                     Notas +
                                   '</' + Prefixo4 + 'ListaRps>';

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('/LoteRps');
    end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsSitLote: String;
var
  strTemp : string;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proCONAM:  begin
                 Gerador.Prefixo := '';
                 Gerador.wGrupo('Sdt_consultaprotocoloin xmlns="NFe"');
                 Gerador.wGrupo('Protocolo>' + Protocolo + '</Protocolo');
                 Gerador.wGrupo('Login');
                 Gerador.wGrupo('CodigoUsuario>' + UserWeb + '</CodigoUsuario');
                 Gerador.wGrupo('CodigoContribuinte>' + SenhaWeb + '</CodigoContribuinte');
                 Gerador.wGrupo('/Login');
                 Gerador.wGrupo('/Sdt_consultaprotocoloin');
               end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupo('prestador');
                    Gerador.wCampo(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampo(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampo(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupo('/prestador');
                    Gerador.wCampo(tcStr, '#1', 'nrLoteRps', 01, 14, 1, NumeroLote, '');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampo(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
             Gerador.wCampo(tcStr, '#2', 'numeroProtocolo', 01, 50, 1, Protocolo, '');
             (*
             Gerador.wGrupo('IdentificacaoPrestador');
             Gerador.wCampo(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampo(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupo('/IdentificacaoPrestador');
             Gerador.wCampo(tcStr, '#1', 'numeroProtocolo', 01, 50, 1, Protocolo, '');
             *)
           end;

    proInfisc,
    proInfiscv11: begin
                    Gerador.Prefixo := '';
                    Gerador.wCampo(tcStr, '#1', 'CNPJ', 14, 14, 1, Cnpj, '');
                    Gerador.wCampo(tcStr, '#1', 'cLote', 01, 15, 1, Protocolo, '');
                  end;

    proISSDSF,
    proSiat: begin 
               // Não possui
             end;

    proNFSEBrasil: begin
                     Gerador.ArquivoFormatoXML := Protocolo;
                   end;

    proSP, 
    proNotaBlu: begin
                  Gerador.wGrupo('Cabecalho' + FaVersao + ' xmlns=""');
                  Gerador.wGrupo('CPFCNPJRemetente');
                  Gerador.wCampoCNPJCPF('', '', Cnpj);
                  Gerador.wGrupo('/CPFCNPJRemetente');
                  Gerador.wCampo(tcStr, '#3', 'NumeroLote', 01, 14, 1, NumeroLote, '');
                  Gerador.wCampo(tcStr, '#4', 'InscricaoPrestador', 01, 15, 1, IM, '');
                  Gerador.wGrupo('/Cabecalho');
                end;

    proPublica: begin
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupo('Prestador ' + Identificador + '="Ass_' +
                                         Cnpj + IM + '"');

                  Gerador.Prefixo := Prefixo4;

                  GerarGrupoCNPJCPF(Cnpj, (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]));

                  if (Provedor <> proBetha) or (IM <> '') then
                    Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');

                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupo('/Prestador');

                  Gerador.wCampo(tcStr, '#4', 'Protocolo', 01, 50, 1, Protocolo, '', True, FaNameSpace);
                end;

    proAssessorPublico:
      begin
        strTemp := '<NFSE><IDENTIFICACAO>'+
                   '<INSCRICAO>'+IM+'</INSCRICAO>'+
                   '<LOTE>'+NumeroLote+'</LOTE>'+
//                                     '<SEQUENCIA>'+NUMERO DA NOTA+'</SEQUENCIA>'+
                   '</IDENTIFICACAO></NFSE>';

//         Gerador.Prefixo := 'nfse:';
        Gerador.wGrupo('Nfse.Execute xmlns="nfse"');
        Gerador.wCampo(tcStr, '', 'Operacao', 1, 1, 1, '3', '');
        Gerador.wCampo(tcStr, '', 'Usuario', 1, 1, 1, UserWeb, '');
        Gerador.wCampo(tcStr, '', 'Senha', 1, 1, 1, SenhaWeb, '');
        Gerador.wCampo(tcStr, '', 'Webxml', 1, 1, 1, strTemp, '');
        Gerador.wGrupo('/Nfse.Execute');
      end;
  else
    begin
      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('Prestador' + FaNameSpace);

      Gerador.Prefixo := Prefixo4;

      GerarGrupoCNPJCPF(Cnpj, (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]));

      if (Provedor <> proBetha) or (IM <> '') then
        Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('/Prestador');

      Gerador.wCampo(tcStr, '#4', 'Protocolo', 01, 50, 1, Protocolo, '', True, FaNameSpace);
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

function TNFSeG.Gera_DadosMsgConsURLNFSe: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';


  Gerador.Prefixo := Prefixo3;
  Gerador.wGrupo('Prestador' + FaNameSpace);

  Gerador.Prefixo := Prefixo4;

  GerarGrupoCNPJCPF(Cnpj, (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]));

  if (not (Provedor in [proBetha, proBethav2])) or (IM <> '') then
    Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');

  Gerador.Prefixo := Prefixo3;
  Gerador.wGrupo('/Prestador');

  Gerador.wCampo(tcStr, '#4', 'Numero', 01, 15, 1, NumeroNFSe, '');
  Gerador.wCampo(tcStr, '#5', 'CodigoTributacaoMunicipio', 01, 20, 0, CodigoTribMun, '');


  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, pro4R, proAgili,
                  proCoplan, profintelISS, proFiorilli, proFriburgo, proGoiania,
                  proGovDigital, proISSDigital, proISSe, proProdata, proVirtual,
                  proSaatri, proFreire, proPVH, proVitoria, proTecnos, proSiam,
                  proSisPMJP, proSystemPro] then
  begin
    Result := '';
  end;
end;

function TNFSeG.Gera_DadosMsgConsLote: String;
var
  strTemp : string;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proAgili,
    proAgiliv2: begin
                  Gerador.wGrupo('IdentificacaoPrestador');
                  Gerador.wCampo(tcStr, '', 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');

                  GerarGrupoCNPJCPF(Cnpj, True);

                  Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                  Gerador.wGrupo('/IdentificacaoPrestador');
                  Gerador.wCampo(tcStr, '#4', 'Protocolo', 01, 50, 1, Protocolo, '', True, FaNameSpace);

                  if VersaoNFSe = ve100 then
                    Gerador.wCampo(tcStr, '', 'Versao', 4, 4, 1, '1.00', '');
                end;

    proCONAM: begin
                Gerador.Prefixo := '';
                Gerador.wGrupo('Sdt_consultanotasprotocoloin xmlns="NFe"');
                Gerador.wGrupo('Protocolo>' + Protocolo + '</Protocolo');
                Gerador.wGrupo('Login');
                Gerador.wGrupo('CodigoUsuario>' + UserWeb + '</CodigoUsuario');
                Gerador.wGrupo('CodigoContribuinte>' + SenhaWeb + '</CodigoContribuinte');
                Gerador.wGrupo('/Login');
                Gerador.wGrupo('/Sdt_consultanotasprotocoloin');
              end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupo('prestador');
                    Gerador.wCampo(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampo(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampo(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupo('/prestador');
                    Gerador.wCampo(tcStr, '#1', 'nrLoteRps', 01, 14, 1, NumeroLote, '');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampo(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
             Gerador.wCampo(tcStr, '#2', 'numeroProtocolo', 01, 50, 1, Protocolo, '');
             (*
             Gerador.wGrupo('IdentificacaoPrestador');
             Gerador.wCampo(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampo(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupo('/IdentificacaoPrestador');
             Gerador.wCampo(tcStr, '#1', 'numeroProtocolo', 01, 50, 1, Protocolo, '');
             *)
           end;

    proInfisc,
    proInfiscv11: begin
                    // Não Possui
                  end;

    proISSDSF,
    proCTA,
    proSiat: begin 
              Gerador.Prefixo := '';
              Gerador.wGrupo('Cabecalho');
              if Provedor = proCTA then
                Gerador.wCampo(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');
              Gerador.wCampo(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
              Gerador.wCampo(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
              Gerador.wCampo(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
              if Provedor = proCTA then  // provedor bugado, pede o NumeroLote mas na verdade exige o Protocolo
                Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 14, 1, Protocolo, '')
              else
                Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 14, 1, NumeroLote, '');
              Gerador.wGrupo('/Cabecalho');
            end;

    proNFSEBrasil: begin
                     Gerador.ArquivoFormatoXML := Protocolo;
                   end;

    proSP,
    proNotaBlu: begin
                  Gerador.wGrupo('Cabecalho' + FaVersao + ' xmlns=""');
                  Gerador.wGrupo('CPFCNPJRemetente');
                  Gerador.wCampoCNPJCPF('', '', Cnpj);
                  Gerador.wGrupo('/CPFCNPJRemetente');

                  Gerador.wCampo(tcStr, '#3', 'NumeroLote', 01, 14, 1, NumeroLote, '');
                  Gerador.wGrupo('/Cabecalho');
                end;

    proSMARAPD: begin
                  Gerador.ArquivoFormatoXML := '<recibo><codrecibo>'+ Protocolo +'</codrecibo></recibo>';
                end;

    proIPM: begin
              Gerador.wGrupo('nfse');
              Gerador.wGrupo('pesquisa');
              Gerador.wCampo(tcStr, '', 'codigo_autenticidade', 01, 16, 0, Protocolo, '');
              Gerador.wCampo(tcStr, '', 'numero', 0, 9, 1, '', '');
              Gerador.wCampo(tcStr, '', 'serie', 0, 1, 1, '', '');
              Gerador.wCampo(tcStr, '', 'cadastro', 0, 9, 1, '', '');
              Gerador.wGrupo('/pesquisa');
              Gerador.wGrupo('/nfse');
            end;

    proAssessorPublico:
      begin
        strTemp := '<NFSE><IDENTIFICACAO>'+
                   '<INSCRICAO>'+IM+'</INSCRICAO>'+
                   '<LOTE>'+NumeroLote+'</LOTE>'+
//                                     '<SEQUENCIA>'+NUMERO DA NOTA+'</SEQUENCIA>'+
                   '</IDENTIFICACAO></NFSE>';

//         Gerador.Prefixo := 'nfse:';
        Gerador.wGrupo('Nfse.Execute xmlns="nfse"');
        Gerador.wCampo(tcStr, '', 'Operacao', 1, 1, 1, '3', '');
        Gerador.wCampo(tcStr, '', 'Usuario', 1, 1, 1, UserWeb, '');
        Gerador.wCampo(tcStr, '', 'Senha', 1, 1, 1, SenhaWeb, '');
        Gerador.wCampo(tcStr, '', 'Webxml', 1, 1, 1, strTemp, '');
        Gerador.wGrupo('/Nfse.Execute');
      end;

     proGeisWeb:
       begin
         Gerador.wCampo(tcStr, '#1', 'CnpjCpf', 01, 15, 1, CNPJ, '');
         Gerador.wGrupo('Consulta');
         Gerador.wCampo(tcStr, '#1', 'CnpjCpfPrestador', 01, 15, 1, CNPJ, '');
         Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');
         Gerador.wGrupo('/Consulta');
       end;
  else
    begin
      if Provedor = proSigep then
      begin
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('credenciais');
        Gerador.wCampo(tcStr, '#01', 'usuario     ', 01, 15, 1, UserWeb);
        Gerador.wCampo(tcStr, '#02', 'senha       ', 01, 05, 1, SenhaWeb);
        Gerador.wCampo(tcStr, '#03', 'chavePrivada', 01, 01, 1, ChaveAcessoPrefeitura);
        Gerador.wGrupo('/credenciais');
      end;

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('Prestador' + FaNameSpace);

      Gerador.Prefixo := Prefixo4;

      GerarGrupoCNPJCPF(Cnpj, (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]));

      if Provedor = proTecnos then
        Gerador.wCampo(tcStr, '#3', 'RazaoSocial', 01, 115, 1, RazaoSocial, '');

      if Provedor = proNFSeBrasil then
        Gerador.wCampo(tcStr, '#3', 'codMunicipio', 01, 07, 1, IntToStr(CodMunicipio), '');

      if (Provedor <> proBetha) or (IM <> '') then
        Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

      if Provedor = proISSDigital then
      begin
        Gerador.wCampo(tcStr, '#5', 'Senha', 06, 10, 1, SenhaWeb, '');
        Gerador.wCampo(tcStr, '#6', 'FraseSecreta', 06, 20, 1, FraseSecreta, '');
      end
      else if Provedor = proAdm then
      begin
        GerarGrupoProAdm;
      end;

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('/Prestador');

      Gerador.wCampo(tcStr, '#4', 'Protocolo', 01, 50, 1, Protocolo, '', True, FaNameSpace);
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
                    Gerador.wCampo(tcStr, '#1', 'UnidadeGestora', 14, 14, 1, CNPJPrefeitura, '');

                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupo('IdentificacaoRps' + FaNameSpace);

                  Gerador.Prefixo := Prefixo4;
                  Gerador.wCampo(tcStr, '#1', 'Numero', 01, 15, 1, NumeroRps, '');
                  Gerador.wCampo(tcStr, '#2', 'Serie', 01, 05, 1, SerieRps, '');

                  case StrToTipoRPS(ok, TipoRps) of
                    trRPS: _TipoRps := '-2';
                    trNFConjugada: _TipoRps := '-4';
                    trCupom: _TipoRps := '-5';
                  end;

                  Gerador.wCampo(tcStr, '#3', 'Tipo', 01, 01, 1, _TipoRps, '');

                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupo('/IdentificacaoRps');

                  Gerador.wGrupo('IdentificacaoPrestador');
                  Gerador.wCampo(tcStr, '' , 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');
                  Gerador.Prefixo := Prefixo4;

                  GerarGrupoCNPJCPF(Cnpj, True);

                  Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wGrupo('/IdentificacaoPrestador');

                  if VersaoNFSe = ve100 then
                    Gerador.wCampo(tcStr, '#5', 'Versao', 4, 4, 1, '1.00', '');
                end;

    proInfisc,
    proInfiscv11: begin
                    // Não Possui
                  end;

    proEquiplano: begin
                    Gerador.Prefixo := '';
                    Gerador.wGrupo('rps');
                    Gerador.wCampo(tcStr, '#1', 'nrRps', 01, 15, 1, NumeroRps, '');
                    Gerador.wCampo(tcStr, '#1', 'nrEmissorRps', 01, 01, 1, '1', '');
                    Gerador.wGrupo('/rps');
                    Gerador.wGrupo('prestador');
                    Gerador.wCampo(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
                    Gerador.wCampo(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
                    Gerador.wCampo(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
                    Gerador.wGrupo('/prestador');
                  end;

    proEL: begin
             Gerador.Prefixo := '';
             Gerador.wCampo(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
             Gerador.wCampo(tcStr, '#2', 'identificacaoRps', 01, 15, 1, NumeroRps, '');
             (*
             Gerador.wGrupo('IdentificacaoPrestador');
             Gerador.wCampo(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampo(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupo('/IdentificacaoPrestador');
             Gerador.wGrupo('IdentificacaoRps');
             Gerador.wCampo(tcStr, '#1', 'Numero', 01, 15, 1, NumeroRps, '');
             Gerador.wCampo(tcStr, '#2', 'Serie', 01, 05, 1, SerieRps, '');
             Gerador.wCampo(tcStr, '#3', 'Tipo', 01, 01, 1, TipoRps, '');
             Gerador.wGrupo('/IdentificacaoRps');
             *)
           end;

    proISSDSF,
    proCTA,
    proSiat: begin 
              Gerador.Prefixo := '';
              Gerador.wGrupo('Cabecalho');
              if Provedor = proCTA then
                Gerador.wCampo(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');
              Gerador.wCampo(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
              Gerador.wCampo(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
              Gerador.wCampo(tcStr, '#1', 'transacao', 01, 14, 1, LowerCase(booltostr(Transacao, True)), '');
              Gerador.wCampo(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
              Gerador.wGrupo('/Cabecalho');

              Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<Lote' + FaIdentificador + '>' +
                                            Notas +
                                          '</Lote>';
            end;

    proGoverna: begin
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupo('ConsultaRps');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wCampo(tcStr, '', 'CodCadBic', 01, 10, 1, IM, '');

                  if CodMunicipio = 3104007  then //Araxá
                    Gerador.wCampo(tcStr, '', 'VrsArq', 01, 01, 1, '4', '')
                  else
                    Gerador.wCampo(tcStr, '', 'VrsArq', 01, 10, 1, '1', '');

                  Gerador.wCampo(tcStr, '', 'ChvAcs', 01, 30, 1, ChaveAcessoPrefeitura, '');
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupo('InfConsultaRPS');
                  Gerador.Prefixo := Prefixo3;
                  Gerador.wCampo(tcStr, '', 'NumRPS', 01, 10, 1, NumeroRps, '');
                  Gerador.wCampo(tcStr, '', 'CodVer', 01, 10, 1, CodVerificacaoRPS, '');
                  Gerador.Prefixo := Prefixo4;
                  Gerador.wGrupo('/InfConsultaRPS');
                  Gerador.wGrupo('/ConsultaRps');
                end;

    proNFSEBrasil: begin
                     // Nenhum valor
                   end;
    proSigIss: begin
                  Gerador.ArquivoFormatoXML := notas;
               end;

    proGiap: begin
               Gerador.wGrupo('consulta');
               Gerador.wCampo(tcStr, '', 'inscricaoMunicipal', 01, 11, 1, IM, '');
               Gerador.wCampo(tcStr, '', 'codigoVerificacao', 01, 02, 1, CodVerificacaoRPS, '');
               Gerador.wGrupo('/consulta');
             end;

    proSP,
    proNotaBlu: begin
                  Gerador.wGrupo('Cabecalho' + FaVersao + ' xmlns=""');
                  Gerador.wGrupo('CPFCNPJRemetente');
                  Gerador.wCampoCNPJCPF('', '', Cnpj);
                  Gerador.wGrupo('/CPFCNPJRemetente');
                  Gerador.wGrupo('/Cabecalho');

                  Gerador.wGrupo('Detalhe xmlns=""');
                  Gerador.wGrupo('ChaveRPS');
                  Gerador.wCampo(tcStr, '', 'InscricaoPrestador', 01, 11, 1, IM, '');
                  Gerador.wCampo(tcStr, '', 'SerieRPS', 01, 02, 1, SerieRPS, '');
                  Gerador.wCampo(tcStr, '', 'NumeroRPS', 01, 12, 1, NumeroRPS, '');
                  Gerador.wGrupo('/ChaveRPS');
                  Gerador.wGrupo('/Detalhe');
                end;
    proIPM: begin
               Gerador.wGrupo('consulta_rps');
               Gerador.wCampo(tcStr, '', 'cidade', 01, 04, 1, CodMunicipioTOM, '');
               Gerador.wCampo(tcStr, '', 'serie_rps', 01, 02, 1, SerieRPS, '');
               Gerador.wCampo(tcStr, '', 'numero_rps', 01, 09, 1, NumeroRPS, '');
               Gerador.wGrupo('/consulta_rps');
            end

  else
    begin
      if Provedor = proSigep then
      begin
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('credenciais');
        Gerador.wCampo(tcStr, '#01', 'usuario     ', 01, 15, 1, UserWeb);
        Gerador.wCampo(tcStr, '#02', 'senha       ', 01, 05, 1, SenhaWeb);
        Gerador.wCampo(tcStr, '#03', 'chavePrivada', 01, 01, 1, ChaveAcessoPrefeitura);
        Gerador.wGrupo('/credenciais');
      end;

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('IdentificacaoRps' + FaNameSpace);

      Gerador.Prefixo := Prefixo4;
      Gerador.wCampo(tcStr, '#1', 'Numero', 01, 15, 1, NumeroRps, '');

      if Provedor <> proMegaSoft then
      begin
        Gerador.wCampo(tcStr, '#2', 'Serie', 01, 05, 1, SerieRps, '');
        Gerador.wCampo(tcStr, '#3', 'Tipo', 01, 01, 1, TipoRps, '');
      end;

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('/IdentificacaoRps');

      Gerador.wGrupo('Prestador' + FaNameSpace);

      Gerador.Prefixo := Prefixo4;

      GerarGrupoCNPJCPF(Cnpj, (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon, pro4R]));

      if Provedor = proTecnos then
        Gerador.wCampo(tcStr, '#3', 'RazaoSocial', 01, 115, 1, RazaoSocial, '');

      if (not (Provedor in [proBetha, proBethav2])) or (IM <> '') then
        Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

      if Provedor = proAsten then
        Gerador.wCampo(tcStr, '#4', 'Token', 01, 30, 1,  ChaveAcessoPrefeitura , '');

      if Provedor = proISSDigital then
      begin
        Gerador.wCampo(tcStr, '#5', 'Senha', 06, 10, 1, SenhaWeb, '');
        Gerador.wCampo(tcStr, '#6', 'FraseSecreta', 06, 20, 1, FraseSecreta, '');
      end
      else if Provedor = proAdm then
      begin
        GerarGrupoProAdm;
      end;

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('/Prestador');

//      if Provedor = proiiBrasilv2 then
//        Gerador.wCampo(tcStr, '', 'Integridade', 01, 2000, 1, Integridade);
    end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proSiam] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgConsNFSe: String;
var
  xAtrib: string;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';
  xAtrib := 'xsi:type="xsd:string"';

  case Provedor of
    proAgili,
    proAgiliv2:
      begin
        Gerador.Prefixo := '';
        if VersaoNFSe = ve100 then
          Gerador.wCampo(tcStr, '', 'UnidadeGestora', 14, 14, 1, CNPJPrefeitura, '');

        Gerador.wGrupo('IdentificacaoPrestador');
        Gerador.wCampo(tcStr, '', 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');

        GerarGrupoCNPJCPF(Cnpj, True);

        Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
        Gerador.wGrupo('/IdentificacaoPrestador');
        Gerador.wCampo(tcStr, '#1', 'NumeroNfseInicial', 01, 15, 1, NumeroNFSe, '');
        Gerador.wCampo(tcStr, '#1', 'NumeroNfseFinal', 01, 15, 1, NumeroNFSe, '');

        if VersaoNFSe = ve100 then
          Gerador.wCampo(tcStr, '#1', 'Versao', 04, 04, 1, '1.00', '');
      end;

    proInfisc,
    proInfiscv11:
      begin
        Gerador.Prefixo := '';
        Gerador.wCampo(tcStr, '#1', 'CNPJ           ', 14, 14, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#1', 'notaInicial    ', 01, 15, 1, NumeroNFSe, '');
        Gerador.wCampo(tcStr, '#1', 'notaFinal      ', 01, 15, 1, NumeroNFSe, '');
        Gerador.wCampo(tcDat, '#1', 'emissaoInicial ', 01, 15, 0, DataInicial, '');
        Gerador.wCampo(tcDat, '#1', 'emissaoFinal   ', 01, 15, 0, DataFinal, '');
        Gerador.wCampo(tcStr, '#1', 'serieNotaFiscal', 01, 15, 0, SerieNFSe, '');
      end;

    proEquiplano:
      begin
        Gerador.Prefixo := '';
        Gerador.wGrupo('prestador');
        Gerador.wCampo(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
        Gerador.wCampo(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
        Gerador.wCampo(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
        Gerador.wGrupo('/prestador');

        if NumeroNFSe <> '' then
          Gerador.wCampo(tcStr, '#1', 'nrNfse', 01, 14, 1, NumeroNFSe, '')
        else
        begin
          Gerador.wGrupo('periodoEmissao');
          Gerador.wCampo(tcDat, '#3', 'dtInicial', 10, 10, 1, DataInicial, '');
          Gerador.wCampo(tcDat, '#4', 'dtFinal', 10, 10, 1, DataFinal, '');
          Gerador.wGrupo('/periodoEmissao');
        end;
      end;

    proEL:
      begin
        Gerador.Prefixo := '';
        Gerador.wCampo(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
        Gerador.wCampo(tcStr, '#2', 'numeroNfse', 01, 15, 1, NumeroNFSe, '');
        Gerador.wCampo(tcDat, '#3', 'dataInicial', 10, 10, 1, DataInicial, '');
        Gerador.wCampo(tcDat, '#4', 'dataFinal', 10, 10, 1, DataFinal, '');
        Gerador.wCampo(tcStr, '#5', 'identificacaoTomador', 11, 14, 1, CNPJTomador, '');
        Gerador.wCampo(tcStr, '#6', 'identificacaoItermediarioServico', 11, 14, 1, CNPJInter, '');

        (*
        Gerador.wGrupo('IdentificacaoPrestador');
        Gerador.wCampo(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
        Gerador.wCampo(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
        Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
        Gerador.wGrupo('/IdentificacaoPrestador');
        Gerador.wCampo(tcStr, '#1', 'numeroNfse', 01, 15, 1, NumeroNFSe, '');
        Gerador.wCampo(tcDat, '#1', 'dataInicial', 10, 10, 1, DataInicial, '');
        Gerador.wCampo(tcDat, '#1', 'dataFinal', 10, 10, 1, DataFinal, '');
        Gerador.wGrupo('IdentificacaoTomador');
        Gerador.wCampo(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJTomador, '');
        Gerador.wCampo(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJTomador)<>14, '1', '2'), '');
        Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IMTomador, '');
        Gerador.wGrupo('/IdentificacaoTomador');
        Gerador.wGrupo('IdentificacaoIntermediario');
        Gerador.wCampo(tcStr, '#1', 'RazaoSocial', 01, 120, 1, NomeInter, '');
        Gerador.wCampo(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJInter, '');
        Gerador.wCampo(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJInter)<>14, '1', '2'), '');
        Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IMInter, '');
        Gerador.wGrupo('/IdentificacaoIntermediario');
        *)
      end;

    proISSDSF,
    proCTA,
    proSiat:
      begin
        Gerador.Prefixo := '';
        Gerador.wGrupo('Cabecalho');

        if Provedor = proCTA then
          Gerador.wCampo(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');

        Gerador.wCampo(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
        Gerador.wCampo(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipalPrestador', 01, 15, 1, IM, '');
        Gerador.wCampo(tcDat, '#1', 'dtInicio', 10, 10, 1, DataInicial, '');
        Gerador.wCampo(tcDat, '#1', 'dtFim', 10, 10, 1, DataFinal, '');

        if Provedor in [proISSDSF, proSiat] then
          Gerador.wCampo(tcStr, '#1', 'NotaInicial', 01, 15, 0, NumeroNFSe, '');

        if Provedor = proSiat then
          Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 12, 1, NumeroLote, '');

        Gerador.wCampo(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
        Gerador.wGrupo('/Cabecalho');
      end;

    proNotaBlu,
    proSP:
      begin
        Gerador.wGrupo('Cabecalho' + FaVersao + ' xmlns=""');
        Gerador.wGrupo('CPFCNPJRemetente');
        Gerador.wCampoCNPJCPF('', '', Cnpj);
        Gerador.wGrupo('/CPFCNPJRemetente');
        Gerador.wGrupo('/Cabecalho');

        Gerador.wGrupo('Detalhe xmlns=""');
        Gerador.wGrupo('ChaveNFe');
        Gerador.wCampo(tcStr, '', 'InscricaoPrestador', 01, 11, 1, IM, '');
        Gerador.wCampo(tcStr, '', 'Numero', 01, 12, 1, NumeroNFSe, '');
//             Gerador.wCampo(tcStr, '', 'CodigoVerificacao', 01, 8, 0, CodVerificacaoRPS, '');
        Gerador.wGrupo('/ChaveNFe');
        Gerador.wGrupo('/Detalhe');
      end;

    proSigISS:
     begin
        Gerador.ArquivoFormatoXML := '';
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('ConsultarNotaPrestador');
        Gerador.wGrupo('DadosPrestador');
        Gerador.wCampo(tcStr, '#01', 'ccm',  01, 015, 0, UserWeb, '');
        Gerador.wCampo(tcStr, '#02', 'cnpj', 11, 014, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#03', 'senha',01, 010, 1, SenhaWeb, '');
        Gerador.wGrupo('/DadosPrestador');
        Gerador.wCampo(tcStr, '', 'nota', 01, 10, 1, OnlyNumber(NumeroNFSe), '');
        Gerador.wGrupo('/ConsultarNotaPrestador');
    end;

    proGoverna:
      begin
       Gerador.Prefixo := Prefixo3;
       Gerador.wCampo(tcStr, '#1', 'CodCadBic', 01, 15, 1, IMTomador, '');
       Gerador.wCampo(tcStr, '#1', 'ChvAcs', 01, 15, 1, SerieNFSe, '');
       Gerador.wCampo(tcStr, '#1', 'NumNot', 01, 15, 1, NumeroNFSe, '');
       Gerador.wCampo(tcStr, '#1', 'CodVer', 01, 15, 1, NomeInter, '');
      end;

    proWebFisco:
      begin
        Gerador.wCampo(tcInt, '', 'usuario', 1,   6, 1, UserWeb, '', True, xAtrib);
        Gerador.wCampo(tcInt, '', 'pass'   , 1,   6, 1, SenhaWeb, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'prf'    , 1,  18, 1, CNPJPrefeitura, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'usr'    , 1,  18, 1, Cnpj, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'ctr'    , 1,   8, 1, NumeroNfse, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'tipo'   , 1,   8, 1, CodigoCanc, '', True, xAtrib);
      end;

     proGeisWeb:
       begin
         Gerador.wCampo(tcStr, '#1', 'CnpjCpf', 01, 15, 1, CNPJ, '');
         Gerador.wGrupo('Consulta');
         Gerador.wCampo(tcStr, '#1', 'CnpjCpfPrestador', 01, 15, 1, CNPJ, '');
         Gerador.wCampo(tcStr, '#1', 'NumeroNfse', 1, 15, 1, NumeroNfse, '');
         Gerador.wCampo(tcDat, '#5', 'DtInicial', 1, 10, 1, DataInicial, '');
         Gerador.wCampo(tcDat, '#6', 'DtFinal', 1, 10, 1, DataFinal, '');
         Gerador.wCampo(tcStr, '#1', 'NumeroInicial', 1, 15, 1, NumeroNfse, '');
         Gerador.wCampo(tcStr, '#1', 'NumeroFinal', 1, 15, 1, NumeroNfse, '');
         Gerador.wCampo(tcInt, '#4', 'Pagina', 01, 06, 1, Pagina, '');
         Gerador.wGrupo('/Consulta');
       end;
  else
    begin
      if Provedor = proSigep then
      begin
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('credenciais');
        Gerador.wCampo(tcStr, '#01', 'usuario     ', 01, 15, 1, UserWeb);
        Gerador.wCampo(tcStr, '#02', 'senha       ', 01, 05, 1, SenhaWeb);
        Gerador.wCampo(tcStr, '#03', 'chavePrivada', 01, 01, 1, ChaveAcessoPrefeitura);
        Gerador.wGrupo('/credenciais');
      end;

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('Prestador' + FaNameSpace);

      Gerador.Prefixo := Prefixo4;

      GerarGrupoCNPJCPF(Cnpj, (VersaoNFSe <> ve100) or (Provedor in [proISSNet, proActcon]));

      if (Provedor <> proBetha) or (IM <> '') then
        Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IM, '');

      if Provedor = proISSDigital then
      begin
        Gerador.wCampo(tcStr, '#5', 'Senha', 06, 10, 1, SenhaWeb, '');
        Gerador.wCampo(tcStr, '#6', 'FraseSecreta', 06, 20, 1, FraseSecreta, '');
      end;

      Gerador.Prefixo := Prefixo3;
      Gerador.wGrupo('/Prestador');

      if NumeroNFSe <> '' then
      begin
        if Provedor in [proPublica, proPVH, proSisPMJP, proSystemPro, proTecnos,
                        proRLZ, proWebISSv2] then
        begin
          Gerador.wGrupo('Faixa');
          Gerador.wCampo(tcStr, '#5', 'NumeroNfseInicial', 01, 15, 1, NumeroNFSe, '');
          Gerador.wCampo(tcStr, '#6', 'NumeroNfseFinal', 01, 15, 1, NumeroNFSe, '');
          Gerador.wGrupo('/Faixa');
        end
        else
          Gerador.wCampo(tcStr, '#5', 'NumeroNfse', 01, 15, 1, NumeroNFSe, '', True, FaNameSpace);
      end;

      if ((DataInicial>0) and (DataFinal>0)) and not
         (provedor in [proPVH, proWebISSv2]) then
      begin
        Gerador.wGrupo('PeriodoEmissao' + FaNameSpace);
        Gerador.wCampo(tcDat, '#5', 'DataInicial', 10, 10, 1, DataInicial, '');
        Gerador.wCampo(tcDat, '#6', 'DataFinal', 10, 10, 1, DataFinal, '');
        Gerador.wGrupo('/PeriodoEmissao');
      end;

      if (CNPJTomador <> '') or (IMTomador <> '')then
      begin
        Gerador.Prefixo := Prefixo3;
        Gerador.wGrupo('Tomador' + FaNameSpace);

        Gerador.Prefixo := Prefixo4;

        GerarGrupoCNPJCPF(CnpjTomador, True);

        Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IMTomador, '');

        Gerador.Prefixo := Prefixo3;
        Gerador.wGrupo('/Tomador');
      (*
      end
      else begin
        if Provedor = proTecnos then
        begin
          Gerador.Prefixo := Prefixo3;
          Gerador.wGrupo('Tomador' + aNameSpace);
          Gerador.Prefixo := Prefixo4;
          Gerador.wGrupo('CpfCnpj');
          Gerador.wCampo(tcStr, '#2', 'Cpf', 11, 11, 1, '', '');
          Gerador.wGrupo('/CpfCnpj');
          Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, '', '');
          Gerador.Prefixo := Prefixo3;
          Gerador.wGrupo('/Tomador');
        end;
      *)
      end;

      if (NomeInter <> '') and (CNPJInter <> '') then
      begin
        Gerador.Prefixo := Prefixo3;
        Gerador.wGrupo('IntermediarioServico' + FaNameSpace);

        Gerador.Prefixo := Prefixo4;
        Gerador.wCampo(tcStr, '#4', 'RazaoSocial', 01, 115, 1, NomeInter, '');

        GerarGrupoCNPJCPF(CnpjInter, (VersaoNFSe <> ve100) or (Provedor in [proActcon, proISSNet]));

        Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, IMInter, '');

        Gerador.Prefixo := Prefixo3;
        Gerador.wGrupo('/IntermediarioServico');
      (*
      end
      else begin
        if Provedor = proTecnos then
        begin
          Gerador.Prefixo := Prefixo3;
          Gerador.wGrupo('IntermediarioServico' + aNameSpace);
          Gerador.Prefixo := Prefixo4;
          Gerador.wCampo(tcStr, '#4', 'RazaoSocial', 01, 115, 1, '', '');
          Gerador.wGrupo('CpfCnpj');
          Gerador.wCampo(tcStr, '#2', 'Cpf', 11, 11, 1, '', '');
          Gerador.wGrupo('/CpfCnpj');
          Gerador.wCampo(tcStr, '#4', 'InscricaoMunicipal', 01, 15, 1, '', '');
          Gerador.Prefixo := Prefixo3;
          Gerador.wGrupo('/IntermediarioServico');
        end;
      *)
      end;

      if Provedor in [proDigifred, profintelISS, proFiorilli, proPronimv2,
                      proPVH, proSisPMJP, proSystemPro, proTecnos, proRLZ,
                      proWebISSv2] then
        Gerador.wCampo(tcInt, '#4', 'Pagina', 01, 06, 1, Pagina, '');
    end;
  end;

  Result := Gerador.ArquivoFormatoXML;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proSiam] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgCancelarNFSe: String;
var
  TagI, TagF, strTemp, xAtrib: string;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';
  xAtrib := 'xsi:type="xsd:string"';

  case Provedor of
    proAdm:
      begin
        Gerador.Prefixo := '';
        Gerador.wGrupo('IdentificacaoNfse');
        Gerador.wCampo(tcStr, '#3', 'Numero', 01, 15, 1, NumeroNfse, '');

        Gerador.Prefixo := '';

        GerarGrupoCNPJCPF(Cnpj, True);

        Gerador.wCampo(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');

        GerarGrupoProAdm;

        Gerador.Prefixo := '';
        Gerador.wGrupo('/IdentificacaoNfse');
      end;

    proAgili,
    proAgiliv2:
      begin
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('IdentificacaoNfse');
        Gerador.wCampo(tcStr, '#3', 'Numero', 01, 15, 1, NumeroNfse, '');

        if VersaoNFSe = ve100 then
        begin
          Gerador.wGrupo('IdentificacaoPrestador');
          Gerador.wCampo(tcStr, '', 'ChaveDigital', 1, 32, 1, ChaveAcessoPrefeitura, '');
        end;

        GerarGrupoCNPJCPF(Cnpj, True);

        Gerador.wCampo(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');

        if VersaoNFSe = ve100 then
          Gerador.wGrupo('/IdentificacaoPrestador');

        Gerador.wGrupo('/IdentificacaoNfse');

        Gerador.wCampo(tcStr, '#1', 'CodigoCancelamento', 01, 01, 1, CodigoCanc, '');

        if VersaoNFSe = ve100 then
        begin
          Gerador.wCampo(tcStr, '#1', 'JustificativaCancelamento', 01, 255, 1, MotivoCanc, '');
          Gerador.wCampo(tcStr, '', 'Versao', 4, 4, 1, '1.00', '');
        end;
      end;

    proCONAM:
      begin
        Gerador.Prefixo := '';
        Gerador.Opcoes.DecimalChar := '.';
        Gerador.wGrupo('Sdt_cancelanfe xmlns="NFe"');

        Gerador.wGrupo('Login');
        Gerador.wCampo(tcStr, '#1', 'CodigoUsuario', 01, 36, 1, UserWeb, '');
        Gerador.wCampo(tcStr, '#2', 'CodigoContribuinte', 01, 36, 1, SenhaWeb, '');
        Gerador.wGrupo('/Login');

        Gerador.wGrupo('Nota');
        Gerador.wCampo(tcStr, '#1', 'SerieNota', 01, 36, 1, SerieNFSe, '');
        Gerador.wCampo(tcStr, '#2', 'NumeroNota', 01, 36, 1, NumeroNfse, '');
        Gerador.wCampo(tcStr, '#3', 'SerieRPS', 01, 36, 1, SerieRps, '');
        Gerador.wCampo(tcStr, '#4', 'NumeroRps', 01, 36, 1, NumeroRps, '');
        Gerador.wCampo(tcStr, '#4', 'ValorNota', 01, 36, 1, FloatToString(ValorNota), '');
        Gerador.wCampo(tcStr, '#4', 'MotivoCancelamento', 01, 36, 1, MotivoCanc, '');
        Gerador.wCampo(tcStr, '#4', 'PodeCancelarGuia', 01, 1, 1, 'S', '');
        Gerador.wGrupo('/Nota');

        Gerador.wGrupo('/Sdt_cancelanfe');
      end;

    proEGoverneISS:
      begin
        Gerador.Prefixo := 'rgm:';

        Gerador.wCampo(tcStr, '#1', 'ChaveAutenticacao', 01, 36, 1, ChaveAcessoPrefeitura, '');
        Gerador.wCampo(tcStr, '#1', 'Homologacao', 04, 05, 1, LowerCase(booltostr(Transacao, True)), '');
        Gerador.wCampo(tcStr, '#1', 'NumeroNota', 01, 15, 1, NumeroNFSe, '');
      end;

    proEquiplano:
      begin
        Gerador.Prefixo := '';

        Gerador.wGrupo('prestador');
        Gerador.wCampo(tcStr, '#1', 'nrInscricaoMunicipal', 01, 15, 1, IM, '');
        Gerador.wCampo(tcStr, '#1', 'cnpj', 14, 14, 1, CNPJ, '');
        Gerador.wCampo(tcStr, '#1', 'idEntidade', 01, 03, 1, GetIdEntidadeEquiplano(CodMunicipio), '');
        Gerador.wGrupo('/prestador');

        Gerador.wCampo(tcStr, '#1', 'nrNfse', 01, 15, 1, NumeroNFSe, '');
        Gerador.wCampo(tcStr, '#1', 'dsMotivoCancelamento', 01, 255, 1, MotivoCanc, '');
      end;

    proEL:
      begin
        Gerador.Prefixo := '';

        Gerador.wCampo(tcStr, '#1', 'identificacaoPrestador', 11, 14, 1, CNPJ, '');
        Gerador.wCampo(tcStr, '#2', 'numeroNfse', 01, 15, 1, NumeroNFSe, '');

             (*
             Gerador.wGrupo('IdentificacaoPrestador');
             Gerador.wCampo(tcStr, '#1', 'CpfCnpj', 11, 14, 1, CNPJ, '');
             Gerador.wCampo(tcStr, '#1', 'IndicacaoCpfCnpj', 01, 01, 1, IfThen(Length(CNPJ)<>14, '1', '2'), '');
             Gerador.wCampo(tcStr, '#1', 'InscricaoMunicipal', 01, 15, 1, IM, '');
             Gerador.wGrupo('/IdentificacaoPrestador');
             Gerador.wCampo(tcStr, '#1', 'numeroNfse', 01, 15, 1, NumeroNFSe, '');
             *)
      end;

    proGinfes:
      begin
        Gerador.Prefixo := '';
        Gerador.wGrupo('Prestador');

        Gerador.Prefixo := Prefixo4;
        Gerador.wCampo(tcStr, '#1', 'Cnpj', 14, 14, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');

        Gerador.Prefixo := '';
        Gerador.wGrupo('/Prestador');
        Gerador.wCampo(tcStr, '#3', 'NumeroNfse', 01, 15, 1, NumeroNfse, '');
      end;

    proGoverna:
      begin
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('LoteCancelamento');

        Gerador.Prefixo := Prefixo3;
        Gerador.wCampo(tcStr, '', 'CodCadBic', 01, 15, 1, IM, '');

        if CodMunicipio = 3104007  then //Araxá
          Gerador.wCampo(tcStr, '', 'VrsArq', 01, 01, 1, '4', '')
        else
          Gerador.wCampo(tcStr, '', 'VrsArq', 01, 01, 1, '1', '');

        Gerador.wCampo(tcStr, '', 'ChvAcs', 30, 30, 1, ChaveAcessoPrefeitura, '');

        Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;

        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('/LoteCancelamento');
      end;

    proInfisc,
    proInfiscv11:
      begin
        Gerador.Prefixo := '';

        Gerador.wCampo(tcStr, '#1', 'CNPJ', 14, 14, 1, Cnpj, '');

        Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
      end;

    proISSDSF,
    proCTA,
    proSiat:
      begin
        Gerador.Prefixo := '';
        Gerador.wGrupo('Cabecalho');

        if Provedor = proCTA then
          Gerador.wCampo(tcStr, '#1', 'TokenEnvio', 32, 32, 1, ChaveAcessoPrefeitura, '');

        Gerador.wCampo(tcStr, '#1', 'CodCidade', 04, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
        Gerador.wCampo(tcStr, '#1', 'CPFCNPJRemetente', 11, 14, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#1', 'transacao', 01, 05, 1, LowerCase(booltostr(Transacao, True)), '');
        Gerador.wCampo(tcStr, '#1', 'Versao', 01, 05, 1, VersaoXML, '');
        Gerador.wGrupo('/Cabecalho');

        Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                          '<Lote' + FaIdentificador + '>' +
                                            Notas +
                                          '</Lote>';
      end;

    proIssCuritiba:
      begin
        Gerador.Prefixo := '';

        Gerador.wGrupo('InfPedidoCancelamento');
        Gerador.wGrupo('IdentificacaoNfse');
        Gerador.wCampo(tcStr, '#1', 'Cnpj', 14, 14, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');
        Gerador.wCampo(tcStr, '#3', 'Numero', 01, 15, 1, NumeroNfse, '');
        Gerador.wGrupo('/IdentificacaoNfse');
        Gerador.wGrupo('/InfPedidoCancelamento');
      end;


    proSigIss:
    begin
        Gerador.ArquivoFormatoXML := Notas;
    end;

    proNFSEBrasil:
      begin
        Gerador.ArquivoFormatoXML := NumeroRps;
      end;

    proGiap: begin
               Gerador.wGrupo('cancelaNota');
               Gerador.wGrupo('numeroNota>' + NumeroNFSe + '</numeroNota');
               Gerador.wGrupo('codigoMotivo>' + CodigoCanc + '</codigoMotivo');
               Gerador.wGrupo('/cancelaNota');
             end;

	  proNotaBlu,
    proSP:
      begin
        Gerador.wGrupo('Cabecalho' + FaVersao + ' xmlns=""');

        Gerador.wGrupo('CPFCNPJRemetente');
        Gerador.wCampoCNPJCPF('', '', Cnpj);
        Gerador.wGrupo('/CPFCNPJRemetente');

        Gerador.wCampo(tcStr, '#3', 'transacao', 01, 05, 0, iif(Transacao,'true','false'), '');

        Gerador.wGrupo('/Cabecalho');

        Gerador.wGrupo('Detalhe xmlns=""');

        Gerador.wGrupo('ChaveNFe');
        Gerador.wCampo(tcStr, '', 'InscricaoPrestador', 01, 11, 1, IM, '');
        Gerador.wCampo(tcStr, '', 'NumeroNFe', 01, 12, 1, NumeroNFSe, '');
        Gerador.wGrupo('/ChaveNFe');

        Gerador.wCampo(tcStr, '', 'AssinaturaCancelamento', 01, 2000, 1, AssinaturaCan, '');
        Gerador.wGrupo('/Detalhe');
      end;

    proSMARAPD:
      begin
        Gerador.wGrupo('nfd');
        Gerador.wGrupo('inscricaomunicipalemissor>' + IM + '</inscricaomunicipalemissor');
        Gerador.wGrupo('numeronf>' + NumeroNFSe + '</numeronf');
        Gerador.wGrupo('motivocancelamento>' + MotivoCanc + '</motivocancelamento');
        Gerador.wGrupo('datacancelamento>' + FormatDateTime('dd/mm/yyyy', now) + '</datacancelamento');
        Gerador.wGrupo('/nfd');
      end;

    proAssessorPublico:
      begin
        strTemp := '<NFSE><IDENTIFICACAO>' +
                   '<INSCRICAO>' + IM + '</INSCRICAO>' +
                   '<LOTE>' + NumeroLote + '</LOTE>' +
                   '<SEQUENCIA>' + NumeroNfse + '</SEQUENCIA>' +
                   '</IDENTIFICACAO></NFSE>';

        Gerador.wGrupo('Nfse.Execute xmlns="nfse"');
        Gerador.wCampo(tcStr, '', 'Operacao', 1, 1, 1, '2', '');
        Gerador.wCampo(tcStr, '', 'Usuario', 1, 1, 1, UserWeb, '');
        Gerador.wCampo(tcStr, '', 'Senha', 1, 1, 1, SenhaWeb, '');
        Gerador.wCampo(tcStr, '', 'Webxml', 1, 1, 1, strTemp, '');
        Gerador.wGrupo('/Nfse.Execute');
      end;

    proWebFisco:
      begin
        Gerador.wCampo(tcInt, '', 'usuario', 1,   6, 1, UserWeb, '', True, xAtrib);
        Gerador.wCampo(tcInt, '', 'pass'   , 1,   6, 1, SenhaWeb, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'prf'    , 1,  18, 1, CNPJPrefeitura, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'usr'    , 1,  18, 1, Cnpj, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'ctr'    , 1,   8, 1, NumeroNfse, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'tipo'   , 1,   8, 1, CodigoCanc, '', True, xAtrib);
        Gerador.wCampo(tcStr, '', 'obs'    , 1, 100, 1, MotivoCanc, '', True, xAtrib);
      end;

    proIPM:
      begin
        Gerador.wGrupo('solicitacao_cancelamento');
        Gerador.wGrupo('prestador');
        Gerador.wCampo(tcStr, '#1', 'cpfcnpj', 01, 15, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#2', 'cidade', 01, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
        Gerador.wGrupo('/prestador');
        Gerador.wGrupo('documentos');
        Gerador.wGrupo('nfse');
        Gerador.wCampo(tcStr, '#1', 'numero', 01, 15, 1, NumeroNfse, '');
        Gerador.wCampo(tcStr, '#2', 'serie', 01, 01, 1, SerieNFSe, '');
        Gerador.wCampo(tcStr, '#3', 'observacao', 01, 01, 1, MotivoCanc, '');
        {
        Gerador.wGrupo('substituta');
        Gerador.wCampo(tcStr, '#1', 'numero', 01, 15, 1, NumeroNfse, '');
        Gerador.wCampo(tcStr, '#2', 'serie', 01, 01, 1, SerieNFSe, '');
        Gerador.wGrupo('/substituta');
        }
        Gerador.wGrupo('/nfse');
        Gerador.wGrupo('/documentos');
        Gerador.wGrupo('/solicitacao_cancelamento');
        {
        Gerador.wGrupo('nfse');
        Gerador.wGrupo('nf');
        Gerador.wCampo(tcStr, '#1', 'numero', 01, 15, 1, NumeroNfse, '');
        Gerador.wCampo(tcStr, '#2', 'situacao', 01, 01, 1, 'C', '');
        Gerador.wCampo(tcStr, '#3', 'observacao', 01, 01, 1, MotivoCanc, '');
        Gerador.wGrupo('/nf');
        Gerador.wGrupo('prestador');
        Gerador.wCampo(tcStr, '#1', 'cpfcnpj', 01, 15, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#2', 'cidade', 01, 04, 1, CodCidadeToCodSiafi(CodMunicipio), '');
        Gerador.wGrupo('/prestador');
        Gerador.wGrupo('/nfse');
        }
      end;

     proGeisWeb:
       begin
         Gerador.wCampo(tcStr, '#1', 'CnpjCpf', 01, 15, 1, CNPJ, '');
         Gerador.wGrupo('Cancela');
         Gerador.wCampo(tcStr, '#1', 'CnpjCpfPrestador', 01, 15, 1, CNPJ, '');
         Gerador.wCampo(tcStr, '#1', 'NumeroNfse', 1, 15, 1, NumeroNfse, '');
         Gerador.wGrupo('/Cancela');
       end;
  else
    begin
      Gerador.Prefixo := Prefixo4;
      Gerador.wGrupo('IdentificacaoNfse');

      if Provedor = proCenti then
        Gerador.wCampo(tcStr, '#4', 'Id', 01, 36, 1, IdCanc, '');

      Gerador.wCampo(tcStr, '#3', 'Numero', 01, 15, 1, NumeroNfse, '');

      GerarGrupoCNPJCPF(Cnpj, (VersaoNFSe <> ve100) or (Provedor in [proActcon, pro4R]));

      if (not (Provedor in [proBetha, proBethav2])) or (IM <> '') then
        Gerador.wCampo(tcStr, '#2', 'InscricaoMunicipal', 01, 15, 1, IM, '');

      if Provedor <> proSigep then
        Gerador.wCampo(tcInt, '#2', 'CodigoMunicipio', 01, 07, 1, CodMunicipio, '');

      if Provedor in [proSigep, proCenti] then
        Gerador.wCampo(tcStr, '#2', 'CodigoVerificacao', 01, 09, 1, CodVerificacaoRPS, '');

      Gerador.wGrupo('/IdentificacaoNfse');

      if Provedor = proSigep then
        Gerador.wCampo(tcStr, '#1', 'CodigoCancelamento', 02, 02, 1, CodigoCanc, '')
      else
        Gerador.wCampo(tcStr, '#1', 'CodigoCancelamento', 01, 01, 1, CodigoCanc, '');

      case Provedor of
        proPublica, proTecnos, proFriburgo, proModernizacaoPublica:
          Gerador.wCampo(tcStr, '#1', 'MotivoCancelamento', 01, 255, 1, MotivoCanc, '');

        proISSNET:
          Gerador.wCampo(tcStr, '#1', 'MotivoCancelamentoNfse', 01, 255, 0, MotivoCanc, '');

        proSigep:
          Gerador.wCampo(tcStr, '#1', 'DescricaoCancelamento', 15, 200, 0, MotivoCanc, '');
      end;
//      if (Provedor in [proPublica]) and (CodigoCanc = 'C999') then
//        Gerador.wCampo(tcStr, '#1', 'MotivoCancelamento', 01, 255, 1, MotivoCanc, '');
    end;
  end;

  case Provedor of
    proAgili,
    proAgiliv2:
      begin
        TagI := '<' + Prefixo3 + 'PedidoCancelamento>';
        TagF := '</' + Prefixo3 + 'PedidoCancelamento>';
      end;

    proISSCuritiba:
      begin
        TagI := '<' + Prefixo3 + 'Pedido>';
        TagF := '</' + Prefixo3 + 'Pedido>';
      end;

    proTecnos,
    proDeISS,
    proFiorilli:
      begin
        TagI := '<' + Prefixo3 + 'Pedido>' +
                   '<' + Prefixo4 + 'InfPedidoCancelamento ' + FaIdentificadorCanc +
                      ' xmlns="http://www.abrasf.org.br/nfse.xsd">';
        TagF :=    '</' + Prefixo4 + 'InfPedidoCancelamento>' +
                '</' + Prefixo3 + 'Pedido>';
      end;

    proRecife,
    proRJ,
    proSimplISS,
    proWebISSv2:
      begin
        TagI := '<' + Prefixo3 + 'Pedido' + FNameSpaceDad + '>' +
                   '<' + Prefixo4 + 'InfPedidoCancelamento' + FaIdentificadorCanc + '>';
        TagF :=    '</' + Prefixo4 + 'InfPedidoCancelamento>' +
                '</' + Prefixo3 + 'Pedido>';
      end;

    proSigep:
      begin
        TagI := '<' + Prefixo4 + 'credenciais>' +
                 '<' + Prefixo4 + 'usuario>' + UserWeb + '</' + Prefixo4 + 'usuario>' +
                 '<' + Prefixo4 + 'senha>' + SenhaWeb + '</' + Prefixo4 + 'senha>' +
                 '<' + Prefixo4 + 'chavePrivada>' + ChaveAcessoPrefeitura + '</' + Prefixo4 + 'chavePrivada>' +
                '</' + Prefixo4 + 'credenciais>' +
                '<' + Prefixo3 + 'Pedido>' +
                   '<' + Prefixo4 + 'InfPedidoCancelamento' + FaIdentificadorCanc + '>';

        TagF :=    '</' + Prefixo4 + 'InfPedidoCancelamento>' +
                '</' + Prefixo3 + 'Pedido>';
      end;

    proSpeedGov:
      begin
        TagI := '<Pedido>' +
                   '<' + Prefixo4 + 'InfPedidoCancelamento' + FaIdentificadorCanc + '>';
        TagF :=    '</' + Prefixo4 + 'InfPedidoCancelamento>' +
                '</Pedido>';
      end;

    proAssessorPublico, proEquiplano, proGinfes, proGoverna, proEGoverneISS,
    proISSDSF, proCTA, proCONAM, proEL, proInfisc, proInfiscv11, proSP,
    proNotaBlu, proSMARAPD, proGiap, proWEBFISCO, proGeisWeb,
    proIPM, proSiat, proSigIss:
      begin
        TagI := '';
        TagF := '';
      end;

    proAdm:
      begin
        TagI := '<Pedido>' +
                 '<InfPedidoCancelamento' + FaIdentificadorCanc + '>';

        TagF :=    '</InfPedidoCancelamento>' +
              '</Pedido>';
      end;

    proElotech:
      begin
        TagI := '<' + Prefixo4 + 'IdentificacaoRequerente>' +
                 '<' + Prefixo4 + 'CpfCnpj>' +
                  '<' + Prefixo4 + 'Cnpj>' + Cnpj + '</' + Prefixo4 + 'Cnpj>' +
                 '</' + Prefixo4 + 'CpfCnpj>' +
                 '<' + Prefixo4 + 'InscricaoMunicipal>' + IM + '</' + Prefixo4 + 'InscricaoMunicipal>' +
                 '<' + Prefixo4 + 'Senha>' + SenhaWeb + '</' + Prefixo4 + 'Senha>' +
                 '<' + Prefixo4 + 'Homologa>' + LowerCase(booltostr(Transacao, True)) + '</' + Prefixo4 + 'Homologa>' +
                '</' + Prefixo4 + 'IdentificacaoRequerente>' +

                '<' + Prefixo3 + 'Pedido>' +
                   '<' + Prefixo4 + 'InfPedidoCancelamento' + FaIdentificadorCanc + '>';

        TagF :=    '</' + Prefixo4 + 'InfPedidoCancelamento>' +
                '</' + Prefixo3 + 'Pedido>';
      end;
  else
    begin
      TagI := '<' + Prefixo3 + 'Pedido>' +
                 '<' + Prefixo4 + 'InfPedidoCancelamento' + FaIdentificadorCanc + '>';
      TagF :=    '</' + Prefixo4 + 'InfPedidoCancelamento>' +
              '</' + Prefixo3 + 'Pedido>';
    end;
  end;

  Gerador.ArquivoFormatoXML := TagI + Gerador.ArquivoFormatoXML + TagF;

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
    proAgili:
      begin
        Gerador.wCampo(tcStr, '', 'UnidadeGestora', 14, 14, 1, CNPJPrefeitura, '');
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

//    proiiBrasilv2:
//      begin
//        Gerador.wCampo(tcStr, '', 'Integridade', 01, 2000, 1, Integridade);

//        Gerador.ArquivoFormatoXML := Notas + Gerador.ArquivoFormatoXML;
//      end;

    proISSDSF,
    proSiat: begin
               // Nao Possui
             end;

    proBHISS,
    proWebISS:
      begin
        Gerador.wGrupo('LoteRps' + FaIdentificador + FaVersao);

        Gerador.Prefixo := Prefixo4;
        Gerador.wCampo(tcStr, '#1', 'NumeroLote', 01, 15, 1, NumeroLote, '');
        Gerador.wCampo(tcStr, '#2', 'Cnpj', 14, 14, 1, Cnpj, '');
        Gerador.wCampo(tcStr, '#3', 'InscricaoMunicipal', 01, 15, 1, IM, '');
        Gerador.wCampo(tcInt, '#4', 'QuantidadeRps', 01, 02, 1, QtdeNotas, '');

        Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                        '<' + Prefixo4 + 'ListaRps>' +
                                          Notas +
                                        '</' + Prefixo4 + 'ListaRps>';

        Gerador.Prefixo := Prefixo3;
        Gerador.wGrupo('/LoteRps');
      end;

    proSP,
    proNotaBlu:
      begin
        Gerador.wGrupo('Cabecalho' + FaVersao + ' xmlns=""');
        Gerador.wGrupo('CPFCNPJRemetente');
        Gerador.wCampoCNPJCPF('', '', Cnpj);
        Gerador.wGrupo('/CPFCNPJRemetente');
        Gerador.wGrupo('/Cabecalho');

        Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
      end;

    proSigep:
      begin
        Gerador.Prefixo := Prefixo4;
        Gerador.wGrupo('credenciais');
        Gerador.wCampo(tcStr, '#01', 'usuario     ', 01, 15, 1, UserWeb);
        Gerador.wCampo(tcStr, '#02', 'senha       ', 01, 05, 1, SenhaWeb);
        Gerador.wCampo(tcStr, '#03', 'chavePrivada', 01, 01, 1, ChaveAcessoPrefeitura);
        Gerador.wGrupo('/credenciais');

        Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Notas;
      end;

    proSiapSistemas:
      begin
        Notas := StringReplace(Notas, '<Rps><InfDeclaracao', '<LoteRps><InfDeclaracao', []);
        Notas := StringReplace(Notas, 'Servico></Rps>', 'Servico></LoteRps>', []);

        Gerador.ArquivoFormatoXML := Notas;
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
                  proTinus, proTiplan, proDSFSJC, proSiat] then 
    Result := '';
end;

function TNFSeG.Gera_DadosMsgEnviarSincrono: String;
begin
  Result := Gera_DadosMsgEnviarLote;

  FPossuiAlertas := (Gerador.ListaDeAlertas.Count <> 0);

  if Provedor in [proNenhum, proABRASFv1, proABRASFv2, proAbaco, proBetha,
                  proBetim, proBHISS, proDBSeller, proEquiplano,
                  proFISSLex, proGinfes, proGoiania, proGovBR, proIssCuritiba,
                  proISSIntel, proISSNet, proLexsom, proNatal,
                  proTinus, proProdemge, proPublica, proRecife, proRJ,
                  proFreire, proSimplISS, proThema, proTiplan, proWebISS,
                  proProdata, proAgili, proSpeedGov, proPronim, proSalvador,
                  proNFSEBrasil] then
    Result := '';
end;

function TNFSeG.Gera_DadosMsgSubstituirNFSe: String;
begin
  Result := Gera_DadosMsgCancelarNFSe;

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
             Gerador.wGrupo('autenticarContribuinte');

             Gerador.Prefixo := Prefixo4;
             Gerador.wCampo(tcStr, '#1', 'identificacaoPrestador', 14, 14, 1, Cnpj, '');
             Gerador.wCampo(tcStr, '#2', 'senha', 01, 14, 1, SenhaWeb, '');

             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupo('/autenticarContribuinte');
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
                  proTinus, proTiplan, proDSFSJC, proSiat] then 
    Result := '';
end;

function TNFSeG.Gera_DadosMsgFecharSessao: String;
begin
  SetAtributos;
  Gerador.ArquivoFormatoXML := '';

  case Provedor of
    proEL: begin
             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupo('finalizarSessao');

             Gerador.Prefixo := Prefixo4;
             Gerador.wCampo(tcStr, '#1', 'hashIdentificador', 01, 40, 1, HashIdent, '');

             Gerador.Prefixo := Prefixo3;
             Gerador.wGrupo('/finalizarSessao');
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
                  proTinus, proTiplan, proDSFSJC, proSiat] then 
    Result := '';
end;

procedure TNFSeG.GerarGrupoCNPJCPF(const CNPJCPF: string; Codicao, GrupoCNPJ: Boolean);
var
  xCNPJ: string;
begin
  if (Codicao) then
  begin
    if GrupoCNPJ then
      Gerador.wGrupo('Cnpj')
    else
      Gerador.wGrupo('CpfCnpj');

    if Length(CNPJCPF) <= 11 then
      Gerador.wCampo(tcStr, '#2', 'Cpf', 11, 11, 1, CNPJCPF, '')
    else
    begin
      xCNPJ := CNPJCPF;

      if Provedor = proadm then
        xCNPJ := FormatarCNPJouCPF(xCNPJ);

      Gerador.wCampo(tcStr, '#2', 'Cnpj', 14, 18, 1, xCNPJ, '');
    end;

    if GrupoCNPJ then
      Gerador.wGrupo('/Cnpj')
    else
      Gerador.wGrupo('/CpfCnpj');
  end
  else
    Gerador.wCampo(tcStr, '#2', 'Cnpj', 14, 14, 1, CNPJCPF, '');
end;

procedure TNFSeG.GerarGrupoProAdm;
begin
  Gerador.wCampo(tcStr, '#4', 'Key', 06, 80, 1, Key, '');
  Gerador.wCampo(tcStr, '#5', 'Auth', 06, 50, 1, Auth, '');
  Gerador.wCampo(tcStr, '#6', 'RequestId', 06, 50, 1, RequestId, '');
end;

end.
