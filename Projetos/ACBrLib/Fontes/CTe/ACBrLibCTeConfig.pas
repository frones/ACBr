{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibCTeConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrCTeConfiguracoes, ACBrCTeDACTeRLClass,
  pcnConversao, ACBrLibComum,
  ACBrLibConfig, DFeReportConfig;

type

  { TDACTeConfig }
  TDACTeConfig = class(TDFeReportConfig<TACBrCTeDACTeRL>)
  private
    FCTeCancelada: Boolean;
    FEPECEnviado: Boolean;
    FImprimirHoraSaida: Boolean;
    FImprimirHoraSaida_Hora: String;
    FProtocoloCTe: String;
    FTamanhoPapel: TpcnTamanhoPapel;
    FTipoDACTe: TpcnTipoImpressao;
    FImprimeDescPorc: Boolean;
    FExibeResumoCanhoto: Boolean;
    FPosCanhoto: TPosRecibo;
    FUsuario: String;
    FImprimirDescPorc: boolean;

  protected
    procedure ImportChild(const AIni: TCustomIniFile); override;
    procedure LerIniChild(const AIni: TCustomIniFile); override;
    procedure GravarIniChild(const AIni: TCustomIniFile); override;
    procedure ApplyChild(const DFeReport: TACBrCTeDACTeRL; const Lib: TACBrLib); override;
    procedure DefinirValoresPadroesChild; override;

  public
    constructor Create;

    property TipoDACTe: TpcnTipoImpressao read FTipoDACTe write FTipoDACTe;
    property ExibeResumoCanhoto: Boolean read FExibeResumoCanhoto write FExibeResumoCanhoto;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto;
    property CTeCancelada: Boolean read FCTeCancelada write FCTeCancelada;
    property EPECEnviado: Boolean read FEPECEnviado write FEPECEnviado;
    property ImprimirHoraSaida: Boolean read FImprimirHoraSaida write FImprimirHoraSaida;
    property ImprimirHoraSaida_Hora: String read FImprimirHoraSaida_Hora write FImprimirHoraSaida_Hora;
    property ProtocoloCTe: String read FProtocoloCTe write FProtocoloCTe;
    property Usuario: String read FUsuario write FUsuario;
    property TamanhoPapel: TpcnTamanhoPapel read FTamanhoPapel write FTamanhoPapel;
    property ImprimeDescPorc: Boolean read FImprimeDescPorc write FImprimeDescPorc;
  end;

  { TLibCTeConfig }
  TLibCTeConfig = class(TLibConfig)
  private
    FDACTeConfig: TDACTeConfig;
    FCTeConfig: TConfiguracoesCTe;

  protected
    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;
    procedure ImportarIni(AIni: TCustomIniFile); override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property CTe: TConfiguracoesCTe read FCTeConfig;
    property DACTe: TDACTeConfig read FDACTeConfig;
  end;

implementation

uses
  blcksock, pcnAuxiliar, pcteConversaoCTe, ACBrDFeSSL,
  ACBrMonitorConsts, ACBrLibConsts, ACBrLibCTeConsts,
  ACBrLibCTeBase, ACBrUtil;

{ TDACTeConfig }
constructor TDACTeConfig.Create;
begin
  inherited Create(CSessaoDACTe);

  DefinirValoresPadroes;
end;

procedure TDACTeConfig.DefinirValoresPadroesChild;
begin
  FTipoDACTe := tiRetrato;
  FImprimeDescPorc := False;
  FExibeResumoCanhoto := False;
  FPosCanhoto := prCabecalho;
  FCTeCancelada := False;
  FEPECEnviado := False;
  FImprimirHoraSaida := False;
  FImprimirHoraSaida_Hora := '';
  FProtocoloCTe := '';
  FUsuario := '';
  FTamanhoPapel := tpA4;
end;

procedure TDACTeConfig.ImportChild(const AIni: TCustomIniFile);
begin
  TamanhoPapel := TpcnTamanhoPapel(AIni.ReadInteger(CSecDACTE, CKeyDACTETamanhoPapel, Integer(TamanhoPapel)));
end;

procedure TDACTeConfig.LerIniChild(const AIni: TCustomIniFile);
begin
  FTipoDACTe := TpcnTipoImpressao(AIni.ReadInteger(FSessao, CChaveTipoDACTe, Integer(FTipoDACTe)));
  FExibeResumoCanhoto := AIni.ReadBool(FSessao, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  FPosCanhoto := TPosRecibo(AIni.ReadInteger(FSessao, CChavePosCanhoto, Integer(FPosCanhoto)));
  FCTeCancelada := AIni.ReadBool(FSessao, CChaveCTeCancelada, FCTeCancelada);
  FEPECEnviado := AIni.ReadBool(FSessao, CChaveEPECEnviado, FEPECEnviado);
  FImprimirHoraSaida := AIni.ReadBool(FSessao, CChaveImprimirHoraSaida, FImprimirHoraSaida);
  FImprimirHoraSaida_Hora := AIni.ReadString(FSessao, CChaveImprimirHoraSaida_Hora, FImprimirHoraSaida_Hora);
  FProtocoloCTe := AIni.ReadString(FSessao, CChaveProtocoloCTe, FProtocoloCTe);
  FUsuario := AIni.ReadString(FSessao, CChaveUsuario, FUsuario);
  FTamanhoPapel := TpcnTamanhoPapel(AIni.ReadInteger(FSessao, CChaveTamanhoPapel, Integer(FTamanhoPapel)));
  FImprimirDescPorc := AIni.ReadBool(FSessao, CChaveImprimirDescPorc, FImprimirDescPorc);
end;

procedure TDACTeConfig.GravarIniChild(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(FSessao, CChaveTipoDACTe, Integer(FTipoDACTe));
  AIni.WriteBool(FSessao, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  AIni.WriteInteger(FSessao, CChavePosCanhoto, Integer(FPosCanhoto));
  AIni.WriteBool(FSessao, CChaveCTeCancelada, FCTeCancelada);
  AIni.WriteBool(FSessao, CChaveEPECEnviado, FEPECEnviado);
  AIni.WriteBool(FSessao, CChaveImprimirHoraSaida, FImprimirHoraSaida);
  AIni.WriteString(FSessao, CChaveImprimirHoraSaida_Hora, FImprimirHoraSaida_Hora);
  AIni.WriteString(FSessao, CChaveProtocoloCTe, FProtocoloCTe);
  AIni.WriteString(FSessao, CChaveUsuario, FUsuario);
  AIni.WriteInteger(FSessao, CChaveTamanhoPapel, Integer(FTamanhoPapel));
  AIni.WriteBool(FSessao, CChaveImprimirDescPorc, FImprimirDescPorc);
end;

procedure TDACTeConfig.ApplyChild(const DFeReport: TACBrCTeDACTeRL; const Lib: TACBrLib);
begin
  with DFeReport do
  begin
    TipoDACTe := FTipoDACTe;
    ImprimeDescPorc := FImprimeDescPorc;
    ExibeResumoCanhoto := FExibeResumoCanhoto;
    PosCanhoto := FPosCanhoto;
    CTeCancelada := FCTeCancelada;
    EPECEnviado := FEPECEnviado;
    ImprimirHoraSaida := FImprimirHoraSaida;
    ImprimirHoraSaida_Hora := FImprimirHoraSaida_Hora;
    ProtocoloCTe := FProtocoloCTe;
    Usuario := FUsuario;
    TamanhoPapel := FTamanhoPapel;
    ImprimeDescPorc := FImprimirDescPorc;
  end;
end;

{ TLibCTeConfig }
constructor TLibCTeConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FCTeConfig := TConfiguracoesCTe.Create(nil);
  FCTeConfig.ChaveCryptINI := AChaveCrypt;

  FDACTeConfig := TDACTeConfig.Create;
end;

destructor TLibCTeConfig.Destroy;
begin
  FCTeConfig.Destroy;
  FDACTeConfig.Free;

  inherited Destroy;
end;

procedure TLibCTeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FCTeConfig.ChaveCryptINI := ChaveCrypt;
  FCTeConfig.LerIni(Ini);
  FDACTeConfig.LerIni(Ini);
end;

procedure TLibCTeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FCTeConfig.ChaveCryptINI := ChaveCrypt;
  FCTeConfig.GravarIni(Ini);
  FDACTeConfig.GravarIni(Ini);
end;

procedure TLibCTeConfig.ClasseParaComponentes;
begin
  FCTeConfig.ChaveCryptINI := ChaveCrypt;

  if Assigned(Owner) then
    TACBrLibCTe(Owner).CTeDM.AplicarConfiguracoes;
end;

procedure TLibCTeConfig.ImportarIni(AIni: TCustomIniFile);
Var
  AuxStr: String;
  Ok: Boolean;
begin
  with CTe.Certificados do
  begin
    //Sessão Certificado
    ArquivoPFX := AIni.ReadString(CSecCertificado, CKeyArquivoPFX, ArquivoPFX);
    NumeroSerie := AIni.ReadString(CSecCertificado, CKeyNumeroSerie, NumeroSerie);

    AuxStr := '';
    AuxStr := AIni.ReadString(CSecCertificado, CKeySenha, '');
    if NaoEstaVazio(AuxStr) then
      Senha := AuxStr;
  end;

  with CTe.Geral do
  begin
    //Sessão Certificado
    SSLCryptLib := TSSLCryptLib(AIni.ReadInteger(CSecCertificado, CKeyCryptLib, Integer(SSLCryptLib)));
    SSLHttpLib := TSSLHttpLib(AIni.ReadInteger(CSecCertificado, CKeyHttpLib, Integer(SSLHttpLib)));
    SSLXmlSignLib := TSSLXmlSignLib(AIni.ReadInteger(CSecCertificado, CKeyXmlSignLib, Integer(SSLXmlSignLib)));

    //ACBrNFeMonitor
    RetirarAcentos := AIni.ReadBool(CSecACBrNFeMonitor, CKeyRetirarAcentos,RetirarAcentos);
    ValidarDigest := AIni.ReadBool(CSecACBrNFeMonitor, CKeyValidarDigest, ValidarDigest);

    //Webservices
    FormaEmissao := TpcnTipoEmissao(AIni.ReadInteger(CSecWebService, CKeyVersaoCTe, Integer(FormaEmissao)));
    VersaoDF := StrToVersaoCTe(Ok, AIni.ReadString(CSecWebService, CKeyVersaoCTe, VersaoCTeToStr(VersaoDF)));
  end;

  with CTe.Arquivos do
  begin
    //ACBrNFeMonitor
    IniServicos := AIni.ReadString(CSecACBrNFeMonitor, CKeyArquivoWebServices, IniServicos);

    //Arquivos
    Salvar := AIni.ReadBool(CSecArquivos, CKeyArquivosSalvar, Salvar);
    SepararPorMes := AIni.ReadBool(CSecArquivos, CKeyArquivosPastaMensal, SepararPorMes);
    SepararPorCNPJ := AIni.ReadBool(CSecArquivos, CKeyArquivosSepararPorCNPJ, SepararPorCNPJ);
    SepararPorModelo := AIni.ReadBool(CSecArquivos, CKeyArquivosSepararPorModelo, SepararPorModelo);
    SepararPorModelo := AIni.ReadBool(CSecArquivos, CKeyArquivosSepararPorModelo, SepararPorModelo);
    AdicionarLiteral := AIni.ReadBool(CSecArquivos, CKeyArquivosAddLiteral, AdicionarLiteral);
    SalvarApenasCTeProcessados := AIni.ReadBool(CSecArquivos, CKeyArquivosSalvarApenasNFesAutorizadas, SalvarApenasCTeProcessados);
    NormatizarMunicipios := AIni.ReadBool(CSecArquivos, CKeyArquivosNormatizarMunicipios, NormatizarMunicipios);
    EmissaoPathCTe := AIni.ReadBool(CSecArquivos, CKeyArquivosEmissaoPathNFe, EmissaoPathCTe);
    PathCTe := AIni.ReadString(CSecArquivos, CKeyArquivosPathNFe, PathCTe);
    PathInu := AIni.ReadString(CSecArquivos, CKeyArquivosPathInu, PathInu);
    PathEvento := AIni.ReadString(CSecArquivos, CKeyArquivosPathEvento, PathEvento);

    AuxStr := AIni.ReadString(CSecArquivos, CKeyArquivosPathSchemasDFe, '');
    if NaoEstaVazio(AuxStr) then
      PathSchemas := PathWithDelim(AuxStr) + 'CTe';

    with DownloadDFe do
    begin
      SepararPorNome := AIni.ReadBool(CSecArquivos, CKeyArquivosSepararPorNome, SepararPorNome);
      PathDownload := AIni.ReadString(CSecArquivos, CKeyArquivosPathDownload, PathDownload);
    end;
  end;

  with CTe.WebServices do
  begin
    // ACBrNFeMonitor
    TimeOut := AIni.ReadInteger(CSecACBrNFeMonitor, CKeyTimeoutWebService, TimeOut);

    // Certificado
    SSLType := TSSLType(AIni.ReadInteger(CSecCertificado, CKeySSLType, Integer(SSLType)));

    //Webservices
    Ambiente := TpcnTipoAmbiente(AIni.ReadInteger(CSecWebService, CKeyAmbiente, Integer(Ambiente)));
    UF := AIni.ReadString(CSecWebService, CKeyUF, UF);
    AjustaAguardaConsultaRet := AIni.ReadBool(CSecWebService, CKeyAjustarAut, AjustaAguardaConsultaRet);
    AguardarConsultaRet := AIni.ReadInteger(CSecWebService, CKeyAguardar, AguardarConsultaRet);
    Tentativas := AIni.ReadInteger(CSecWebService, CKeyTentativas, Tentativas);
    IntervaloTentativas := AIni.ReadInteger(CSecWebService, CKeyWebServiceIntervalo, IntervaloTentativas);

    with TimeZoneConf do
    begin
      ModoDeteccao := TTimeZoneModoDeteccao(AIni.ReadInteger(CSecWebService, CKeyTimeZoneMode, Integer(ModoDeteccao)));
      TimeZoneStr := AIni.ReadString(CSecWebService, CKeyTimeZoneStr, TimeZoneStr);
    end;
  end;

  with CTe.RespTec do
  begin
    // RespTecnico
    IdCSRT := AIni.ReadInteger(CSecRespTecnico, CKeyidCSRT, IdCSRT);
    CSRT := AIni.ReadString(CSecRespTecnico, CKeyCSRT, CSRT);
  end;

  //Impressão
  DACTe.Import(AIni);
end;

procedure TLibCTeConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCTe(Owner) do
      CTeDM.Travar;
  end;
end;

procedure TLibCTeConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCTe(Owner) do
      CTeDM.Destravar;
  end;
end;

end.

