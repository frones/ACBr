{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: José M. S. Junior                               }
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

unit ACBrLibBoletoConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrBoleto, ACBrBoletoConversao, ACBrLibConfig, ACBrLibComum, pcnConversao, ACBrDFeConfiguracoes, ACBrPIXBase;

type

  { TBoletoDiretorioConfig }
  TBoletoDiretorioConfig = class
  private
    FDataArquivo: TDateTime;
    FDataCreditoLanc: TDateTime;
    FDirArqRemessa: String;
    FDirArqRetorno: String;
    FDirHomologacao: Boolean;
    FImprimirMensagemPadrao: Boolean;
    FLayoutRemessa: TACBrLayoutRemessa;
    FLeCedenteRetorno: Boolean;
    FLerNossoNumeroCompleto: Boolean;
    FNomeArqRemessa: String;
    FNomeArqRetorno: String;
    FNumeroArquivo: Integer;
    FRemoveAcentosArqRemessa: Boolean;
    FPrefixArqRemessa: String;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property DataArquivo: TDateTime read FDataArquivo write FDataArquivo;
    property DataCreditoLanc: TDateTime read FDataCreditoLanc write FDataCreditoLanc;
    property DirArqRemessa: String read FDirArqRemessa write FDirArqRemessa;
    property DirArqRetorno: String read FDirArqRetorno write FDirArqRetorno;
    property DirHomologacao: Boolean read FDirHomologacao write FDirHomologacao;
    property ImprimirMensagemPadrao: Boolean read FImprimirMensagemPadrao write FImprimirMensagemPadrao;
    property LayoutRemessa: TACBrLayoutRemessa read FLayoutRemessa write FLayoutRemessa;
    property LeCedenteRetorno: Boolean read FLeCedenteRetorno write FLeCedenteRetorno;
    property LerNossoNumeroCompleto: Boolean read FLerNossoNumeroCompleto write FLerNossoNumeroCompleto;
    property NomeArqRemessa: String read FNomeArqRemessa write FNomeArqRemessa;
    property NomeArqRetorno: String read FNomeArqRetorno write FNomeArqRetorno;
    property NumeroArquivo: Integer read FNumeroArquivo write FNumeroArquivo;
    property RemoveAcentosArqRemessa: Boolean read FRemoveAcentosArqRemessa write FRemoveAcentosArqRemessa;
    property PrefixArqRemessa: String read FPrefixArqRemessa write FPrefixArqRemessa;

  end;

  { TBoletoBancoConfig  }
  TBoletoBancoConfig = class
  private
    FDigito: Integer;
    FLayoutVersaoArquivo: Integer;
    FLayoutVersaoLote: Integer;
    FLocalPagamento: String;
    FNumero: Integer;
    FNumeroCorrespondente: Integer;
    FOrientacaoBanco: String;
    FTipoCobranca: TACBrTipoCobranca;
    FCIP: string;
    FDensidadeGravacao: string;
    FCasasDecimaisMoraJuros: Integer;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Digito: Integer read FDigito write FDigito;
    property LayoutVersaoArquivo: Integer read FLayoutVersaoArquivo write FLayoutVersaoArquivo;
    property LayoutVersaoLote: Integer read FLayoutVersaoLote write FLayoutVersaoLote;
    property LocalPagamento: String read FLocalPagamento write FLocalPagamento;
    property Numero: Integer read FNumero write FNumero;
    property NumeroCorrespondente: Integer read FNumeroCorrespondente write FNumeroCorrespondente;
    property OrientacaoBanco: String read FOrientacaoBanco write FOrientacaoBanco;
    property TipoCobranca: TACBrTipoCobranca read FTipoCobranca write FTipoCobranca;
    property CasasDecimaisMoraJuros: Integer read FCasasDecimaisMoraJuros write FCasasDecimaisMoraJuros;
    property DensidadeGravacao : string read FDensidadeGravacao write FDensidadeGravacao;
    property CIP: string read FCIP write FCIP;

  end;

  {TBoletoCedenteWS}
  TBoletoCedenteWS = class
  private
    FClientID: String;
    FClientSecret: String;
    FKeyUser: String;
    FScope: String;
    FIndicadorPix: Boolean;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ClientID: String read FClientID write FClientID;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property KeyUser: String read FKeyUser write FKeyUser;
    property Scope: String read FScope write FScope;
    property IndicadorPix: Boolean read FIndicadorPix write FIndicadorPix;

  end;

  { TBoletoCedenteConfig }
  TBoletoCedenteConfig = class
  private
    FAgencia: String;
    FAgenciaDigito: String;
    FBairro: String;
    FCaracTitulo: TACBrCaracTitulo;
    FCEP: String;
    FCidade: String;
    FCNPJCPF: String;
    FCodigoCedente: String;
    FCodigoTransmissao: String;
    FComplemento: String;
    FConta: String;
    FContaDigito: String;
    FConvenio: String;
    FLogradouro: String;
    FModalidade: String;
    FNome: String;
    FNumeroRes: String;
    FResponEmissao: TACBrResponEmissao;
    FTelefone: String;
    FTipoCarteira: TACBrTipoCarteira;
    FTipoDocumento: TACBrTipoDocumento;
    FTipoInscricao: TACBrPessoaCedente;
    FUF: String;
    FDigitoVerificadorAgenciaConta: String;
    FIdentDistribuicao: TACBrIdentDistribuicao;
    FOperacao: String;
    FPIXChave: String;
    FPIXTipoChave : TACBrPIXTipoChave;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Nome         : String read FNome   write FNome;
    property CodigoCedente: String read FCodigoCedente write FCodigoCedente;
    property CodigoTransmissao : String read fCodigoTransmissao write FCodigoTransmissao;
    property Agencia      : String read FAgencia       write FAgencia;
    property AgenciaDigito: String read FAgenciaDigito write FAgenciaDigito;
    property Conta        : String read FConta         write FConta;
    property ContaDigito  : String read FContaDigito   write FContaDigito;
    property Modalidade   : String read FModalidade    write FModalidade;
    property Convenio     : String read FConvenio      write FConvenio;
    property TipoDocumento : TACBrTipoDocumento read FTipoDocumento write FTipoDocumento;
    property TipoCarteira : TACBrTipoCarteira read FTipoCarteira write FTipoCarteira;
    property ResponEmissao: TACBrResponEmissao read FResponEmissao  write FResponEmissao;
    property CaracTitulo: TACBrCaracTitulo read FCaracTitulo  write FCaracTitulo;
    property CNPJCPF      : String  read FCNPJCPF  write FCNPJCPF;
    property TipoInscricao: TACBrPessoaCedente  read FTipoInscricao write FTipoInscricao;
    property Logradouro  : String  read FLogradouro  write FLogradouro;
    property NumeroRes   : String  read FNumeroRes      write FNumeroRes;
    property Complemento : String  read FComplemento write FComplemento;
    property Bairro      : String  read FBairro      write FBairro;
    property Cidade      : String  read FCidade      write FCidade;
    property UF          : String  read FUF          write FUF;
    property CEP         : String  read FCEP         write FCEP;
    property Telefone    : String  read FTelefone    write FTelefone;
    property DigitoVerificadorAgenciaConta : String read FDigitoVerificadorAgenciaConta   write FDigitoVerificadorAgenciaConta;
    property IdentDistribuicao: TACBrIdentDistribuicao read FIdentDistribuicao  write FIdentDistribuicao;
    property Operacao: string read FOperacao write FOperacao;
    property PIXChave         :String read FPIXChave write FPIXChave;
    property PIXTipoChave     : TACBrPIXTipoChave read FPIXTipoChave write FPIXTipoChave;

  end;

  { TBoletoFCFortesConfig }
  TBoletoFCFortesConfig = class
  private
    FDirLogo: String;
    FFiltro: TACBrBoletoFCFiltro;
    FLayout: TACBrBolLayOut;
    FMargemDireita: double;
    FMargemEsquerda: double;
    FMargemInferior: double;
    FMargemSuperior: double;
    FMostrarPreview: Boolean;
    FMostrarProgresso: Boolean;
    FMostrarSetup: Boolean;
    FNomeArquivo: String;
    FNumeroCopias: Integer;
    FPrinterName: String;
    FSoftwareHouse: String;
    FAlterarEscalaPadrao: Boolean;
    FNovaEscala: Integer;
    FCalcularNomeArquivoPDFIndividual: Boolean;
  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property DirLogo: String read FDirLogo write FDirLogo;
    property Filtro: TACBrBoletoFCFiltro read FFiltro write FFiltro;
    property Layout: TACBrBolLayOut read FLayout write FLayout;
    property MostrarPreview: Boolean read FMostrarPreview write FMostrarPreview;
    property MostrarProgresso: Boolean read FMostrarProgresso write FMostrarProgresso;
    property MostrarSetup: Boolean read FMostrarSetup write FMostrarSetup;
    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property NumeroCopias: Integer read FNumeroCopias write FNumeroCopias;
    property PrinterName: String read FPrinterName write FPrinterName;
    property SoftwareHouse: String read FSoftwareHouse write FSoftwareHouse;
    property AlterarEscalaPadrao: Boolean read FAlterarEscalaPadrao write FAlterarEscalaPadrao;
    property NovaEscala: Integer read FNovaEscala write FNovaEscala;
    property CalcularNomeArquivoPDFIndividual: Boolean read FCalcularNomeArquivoPDFIndividual write FCalcularNomeArquivoPDFIndividual;
    property MargemInferior  : double read FMargemInferior   write FMargemInferior;
    property MargemSuperior  : double read FMargemSuperior   write FMargemSuperior;
    property MargemEsquerda  : double read FMargemEsquerda   write FMargemEsquerda;
    property MargemDireita   : double read FMargemDireita    write FMargemDireita;

  end;

  { TBoletoConfigWS }
  TBoletoConfigWS = class
  private
    FLogRegistro: Boolean;
    FPathGravarRegistro: String;
    FOperacao: TOperacao;
    FVersaoDF: String;
    FUseCertificateHTTP: Boolean;
    FArquivoCRT: String;
    FArquivoKEY: String;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property LogRegistro: Boolean read FLogRegistro write FLogRegistro;
    property PathGravarRegistro: String read FPathGravarRegistro write FPathGravarRegistro;
    property Operacao: TOperacao read FOperacao write FOperacao;
    property VersaoDF: String read FVersaoDF write FVersaoDF;
    property UseCertificateHTTP: Boolean read FUseCertificateHTTP write FUseCertificateHTTP;
    property ArquivoCRT: String read FArquivoCRT write FArquivoCRT;
    property ArquivoKEY: String read FArquivoKEY write FArquivoKEY;

  end;

  { TBoletoConfig }
  TBoletoConfig = class
  private
    FemailAssuntoBoleto  : String ;
    FemailMensagemBoleto : String ;

  public
    constructor Create;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property emailAssuntoBoleto: String read FemailAssuntoBoleto write FEmailAssuntoBoleto;
    property emailMensagemBoleto: String read FemailMensagemBoleto write FemailMensagemBoleto;

  end;

  { TLibBoletoConfig }
  TLibBoletoConfig = class(TLibConfig)
  private
    FBoletoDiretorioConfig: TBoletoDiretorioConfig;
    FBoletoBancoConfig: TBoletoBancoConfig;
    FBoletoCedenteConfig: TBoletoCedenteConfig;
    FBoletoFCFortesConfig: TBoletoFCFortesConfig;
    FBoletoConfig: TBoletoConfig;
    FBoletoCedenteWS: TBoletoCedenteWS;
    FBoletoConfigWS: TBoletoConfigWS;
    FBoletoDFeConfigWS: TConfiguracoes;

  protected
    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: string = ''; AChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    function AjustarValor(Tipo: TTipoFuncao; ASessao, AChave, AValor: Ansistring): Ansistring; override;

    property BoletoDiretorioConfig: TBoletoDiretorioConfig read FBoletoDiretorioConfig;
    property BoletoBancoConfig: TBoletoBancoConfig read FBoletoBancoConfig;
    property BoletoCedenteConfig: TBoletoCedenteConfig read FBoletoCedenteConfig;
    property BoletoFCFortesConfig: TBoletoFCFortesConfig read FBoletoFCFortesConfig;
    property BoletoConfig: TBoletoConfig read FBoletoConfig;
    property BoletoCedenteWS: TBoletoCedenteWS read FBoletoCedenteWS;
    property BoletoConfigWS: TBoletoConfigWS read FBoletoConfigWS;
    property BoletoDFeConfigWS: TConfiguracoes read FBoletoDFeConfigWS;

  end;


implementation

uses
  typinfo, strutils, synacode, blcksock, ACBrLibBoletoConsts,
  ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrConsts, ACBrLibConsts, ACBrLibBoletoBase;

{ TBoletoConfigWS }

constructor TBoletoConfigWS.Create;
begin
  FLogRegistro:= True;
  FPathGravarRegistro:= '';
  FOperacao:= tpInclui;
  FVersaoDF:= '1.2';
  FUseCertificateHTTP:= False;
  FArquivoCRT:= '';
  FArquivoKEY:= '';

end;

procedure TBoletoConfigWS.LerIni(const AIni: TCustomIniFile);
begin
  LogRegistro:= AIni.ReadBool(CSessaoBoletoWebService, CChaveLogRegistro, LogRegistro );
  PathGravarRegistro:= AIni.ReadString(CSessaoBoletoWebService, CChavePathGravarRegistro, PathGravarRegistro );
  Operacao:= TOperacao( AIni.ReadInteger(CSessaoBoletoWebService, CChaveOperacao, integer(Operacao) ) );
  VersaoDF:= AIni.ReadString(CSessaoBoletoWebService, CChaveVersaoDF, VersaoDF );
  UseCertificateHTTP:= AIni.ReadBool(CSessaoBoletoWebService, CChaveUseCertificateHTTP, UseCertificateHTTP );
  ArquivoCRT:= AIni.ReadString(CSessaoBoletoWebService, CChaveArquivoCRT, ArquivoCRT);
  ArquivoKEY:= AIni.ReadString(CSessaoBoletoWebService, CChaveArquivoKEY, ArquivoKEY);
end;

procedure TBoletoConfigWS.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteBool(CSessaoBoletoWebService, CChaveLogRegistro, LogRegistro );
  AIni.WriteString(CSessaoBoletoWebService, CChavePathGravarRegistro, PathGravarRegistro );
  AIni.WriteInteger(CSessaoBoletoWebService, CChaveOperacao, integer(Operacao) );
  AIni.WriteString(CSessaoBoletoWebService, CChaveVersaoDF, VersaoDF );
  AIni.WriteBool(CSessaoBoletoWebService, CChaveUseCertificateHTTP, UseCertificateHTTP );
  AIni.WriteString(CSessaoBoletoWebService, CChaveArquivoCRT, ArquivoCRT);
  AIni.WriteString(CSessaoBoletoWebService, CChaveArquivoKEY, ArquivoKEY);
end;

{ TBoletoCedenteWS }

constructor TBoletoCedenteWS.Create;
begin
  FClientID:= '';
  FClientSecret:= '';
  FKeyUser:= '';
  FScope:= '';
  FIndicadorPix:= False;
end;

procedure TBoletoCedenteWS.LerIni(const AIni: TCustomIniFile);
begin
  ClientID:= AIni.ReadString(CSessaoBoletoCedenteWS, CChaveClientID, ClientID );
  ClientSecret:= AIni.ReadString(CSessaoBoletoCedenteWS, CChaveClientSecret, ClientSecret);
  KeyUser:= AIni.ReadString(CSessaoBoletoCedenteWS, CChaveKeyUser, KeyUser);
  Scope:= AIni.ReadString(CSessaoBoletoCedenteWS, CChaveScope, Scope);
  IndicadorPix:= AIni.ReadBool(CSessaoBoletoCedenteWS, CChaveIndicadorPix, IndicadorPix);
end;

procedure TBoletoCedenteWS.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoBoletoCedenteWS, CChaveClientID, ClientID  );
  AIni.WriteString(CSessaoBoletoCedenteWS, CChaveClientSecret, ClientSecret  );
  AIni.WriteString(CSessaoBoletoCedenteWS, CChaveKeyUser, KeyUser  );
  AIni.WriteString(CSessaoBoletoCedenteWS, CChaveScope, Scope  );
  AIni.WriteBool(CSessaoBoletoCedenteWS, CChaveIndicadorPix, IndicadorPix);
end;

{ TLibBoletoConfig }
procedure TLibBoletoConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FBoletoDiretorioConfig.LerIni(Ini);
  FBoletoBancoConfig.LerIni(Ini);
  FBoletoCedenteConfig.LerIni(Ini);
  FBoletoFCFortesConfig.LerIni(Ini);
  FBoletoConfig.LerIni(Ini);
  FBoletoCedenteWS.LerIni(Ini);
  FBoletoConfigWS.LerIni(Ini);
  FBoletoDFeConfigWS.ChaveCryptINI:= ChaveCrypt;
  FBoletoDFeConfigWS.LerIni(Ini);

end;

procedure TLibBoletoConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FBoletoDiretorioConfig.GravarIni(Ini);
  FBoletoBancoConfig.GravarIni(Ini);
  FBoletoCedenteConfig.GravarIni(Ini);
  FBoletoFCFortesConfig.GravarIni(Ini);
  FBoletoConfig.GravarIni(Ini);
  FBoletoCedenteWS.GravarIni(Ini);
  FBoletoConfigWS.GravarIni(Ini);
  FBoletoDFeConfigWS.ChaveCryptINI:= ChaveCrypt;
  FBoletoDFeConfigWS.GravarIni(Ini);

end;

procedure TLibBoletoConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibBoleto(Owner).BoletoDM.AplicarConfiguracoes;
end;

procedure TLibBoletoConfig.Travar;
begin
  FBoletoDFeConfigWS.ChaveCryptINI:= ChaveCrypt;

  if Assigned(Owner) then
  begin
    with TACBrLibBoleto(Owner) do
      BoletoDM.Travar;
  end;
end;

procedure TLibBoletoConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibBoleto(Owner) do
      BoletoDM.Destravar;
  end;
end;

constructor TLibBoletoConfig.Create(AOwner: TObject; ANomeArquivo: string;
  AChaveCrypt: ansistring);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FBoletoDiretorioConfig := TBoletoDiretorioConfig.Create;
  FBoletoBancoConfig := TBoletoBancoConfig.Create;
  FBoletoCedenteConfig := TBoletoCedenteConfig.Create;
  FBoletoFCFortesConfig := TBoletoFCFortesConfig.Create;
  FBoletoConfig := TBoletoConfig.Create;
  FBoletoCedenteWS := TBoletoCedenteWS.Create;
  FBoletoConfigWS := TBoletoConfigWS.Create;
  FBoletoDFeConfigWS := TConfiguracoes.CreateNomearSessao(nil, CSessaoBoletoWebService);
  FBoletoDFeConfigWS.ChaveCryptINI := AChaveCrypt;

end;

destructor TLibBoletoConfig.Destroy;
begin
  FBoletoDiretorioConfig.Free;
  FBoletoBancoConfig.Free;
  FBoletoCedenteConfig.Free;
  FBoletoFCFortesConfig.Free;
  FBoletoConfig.Free;
  FBoletoCedenteWS.Free;
  FBoletoConfigWS.Free;
  FBoletoDFeConfigWS.Free;

  inherited Destroy;
end;

{ TBoletoConfig }
constructor TBoletoConfig.Create;
begin
  FemailAssuntoBoleto  := '';
  FemailMensagemBoleto := '';
end;

procedure TBoletoConfig.LerIni(const AIni: TCustomIniFile);
begin
  emailAssuntoBoleto:= AIni.ReadString(CSessaoBolConfig, CChaveemailAssuntoBoleto, emailAssuntoBoleto );
  emailMensagemBoleto:= AIni.ReadString(CSessaoBolConfig, CChaveemailMensagemBoleto, emailMensagemBoleto );

end;

procedure TBoletoConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoBolConfig, CChaveemailAssuntoBoleto, emailAssuntoBoleto  );
  AIni.WriteString(CSessaoBolConfig, CChaveemailMensagemBoleto, emailMensagemBoleto  );
end;

{ TBoletoFCFortesConfig }
constructor TBoletoFCFortesConfig.Create;
begin
  FDirLogo:= '';
  FFiltro:= fiNenhum;
  FLayout:= lPadrao;
  FMostrarPreview:= False;
  FMostrarProgresso:= False;
  FMostrarSetup:= False;
  FNomeArquivo:= '';
  FNumeroCopias:= 1;
  FPrinterName:= '';
  FSoftwareHouse:= '';
  FAlterarEscalaPadrao:= False;
  FNovaEscala:= 96;
  FCalcularNomeArquivoPDFIndividual:= False;
  FMargemInferior:=5;
  FMargemSuperior:=5;
  FMargemEsquerda:=4;
  FMargemDireita:=3;
end;

procedure TBoletoFCFortesConfig.LerIni(const AIni: TCustomIniFile);
begin
  DirLogo:= AIni.ReadString(CSessaoBoletoFCFortesConfig, CChaveDirLogo, DirLogo );
  Filtro:= TACBrBoletoFCFiltro( AIni.Readinteger(CSessaoBoletoFCFortesConfig, CChaveFiltro, integer(Filtro)) );
  Layout:= TACBrBolLayOut( AIni.ReadInteger(CSessaoBoletoFCFortesConfig, CChaveLayout, integer(Layout)) );
  MostrarPreview:= AIni.ReadBool(CSessaoBoletoFCFortesConfig, CChaveMostrarPreview, MostrarPreview );
  MostrarProgresso:= AIni.ReadBool(CSessaoBoletoFCFortesConfig, CChaveMostrarProgresso, MostrarProgresso );
  MostrarSetup:= AIni.ReadBool(CSessaoBoletoFCFortesConfig, CChaveMostrarSetup, MostrarSetup );
  NomeArquivo:= AIni.ReadString(CSessaoBoletoFCFortesConfig, CChaveNomeArquivo, NomeArquivo );
  NumeroCopias:= AIni.ReadInteger(CSessaoBoletoFCFortesConfig, CChaveNumeroCopias, NumeroCopias );
  PrinterName:= AIni.ReadString(CSessaoBoletoFCFortesConfig, CChavePrinterName, PrinterName);
  SoftwareHouse:= AIni.ReadString(CSessaoBoletoFCFortesConfig, CChaveSoftwareHouse, SoftwareHouse );
  AlterarEscalaPadrao:= AIni.ReadBool(CSessaoBoletoFCFortesConfig, CChaveAlterarEscalaPadrao, AlterarEscalaPadrao );
  NovaEscala:= AIni.ReadInteger(CSessaoBoletoFCFortesConfig, CChaveNovaEscala, NovaEscala);
  CalcularNomeArquivoPDFIndividual := AIni.ReadBool(CSessaoBoletoFCFortesConfig, CChaveCalcularNomeArquivoPDFIndividual, CalcularNomeArquivoPDFIndividual);
  MargemInferior:= AIni.ReadFloat(CSessaoBoletoFCFortesConfig, CChaveMargemInferior, MargemInferior );
  MargemSuperior:= AIni.ReadFloat(CSessaoBoletoFCFortesConfig, CChaveMargemSuperior, MargemSuperior );
  MargemEsquerda:= AIni.ReadFloat(CSessaoBoletoFCFortesConfig, CChaveMargemEsquerda, MargemEsquerda );
  MargemDireita := AIni.ReadFloat(CSessaoBoletoFCFortesConfig, CChaveMargemDireita,  MargemDireita );


end;

procedure TBoletoFCFortesConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoBoletoFCFortesConfig, CChaveDirLogo, DirLogo  );
  AIni.WriteInteger(CSessaoBoletoFCFortesConfig, CChaveFiltro, integer(Filtro)  );
  AIni.WriteInteger(CSessaoBoletoFCFortesConfig, CChaveLayout, integer(Layout) );
  AIni.WriteBool(CSessaoBoletoFCFortesConfig, CChaveMostrarPreview, MostrarPreview  );
  AIni.WriteBool(CSessaoBoletoFCFortesConfig, CChaveMostrarProgresso, MostrarProgresso  );
  AIni.WriteBool(CSessaoBoletoFCFortesConfig, CChaveMostrarSetup, MostrarSetup  );
  AIni.WriteString(CSessaoBoletoFCFortesConfig, CChaveNomeArquivo, NomeArquivo  );
  AIni.WriteInteger(CSessaoBoletoFCFortesConfig, CChaveNumeroCopias, NumeroCopias  );
  AIni.WriteString(CSessaoBoletoFCFortesConfig, CChavePrinterName, PrinterName  );
  AIni.WriteString(CSessaoBoletoFCFortesConfig, CChaveSoftwareHouse, SoftwareHouse  );
  AIni.WriteBool(CSessaoBoletoFCFortesConfig, CChaveAlterarEscalaPadrao, AlterarEscalaPadrao);
  AIni.WriteInteger(CSessaoBoletoFCFortesConfig, CChaveNovaEscala, NovaEscala );
  AIni.WriteBool(CSessaoBoletoFCFortesConfig, CChaveCalcularNomeArquivoPDFIndividual, CalcularNomeArquivoPDFIndividual );
  AIni.WriteFloat(CSessaoBoletoFCFortesConfig, CChaveMargemInferior, MargemInferior  );
  AIni.WriteFloat(CSessaoBoletoFCFortesConfig, CChaveMargemSuperior, MargemSuperior  );
  AIni.WriteFloat(CSessaoBoletoFCFortesConfig, CChaveMargemEsquerda, MargemEsquerda  );
  AIni.WriteFloat(CSessaoBoletoFCFortesConfig, CChaveMargemDireita,  MargemDireita  );

end;

{ TBoletoCedenteConfig }
constructor TBoletoCedenteConfig.Create;
begin
  FAgencia:= '';
  FAgenciaDigito:= '';
  FBairro:= '';
  FCaracTitulo:= tcSimples;
  FCEP:= '';
  FCidade:= '';
  FCNPJCPF:= '';
  FCodigoCedente:= '';
  FCodigoTransmissao:= '';
  FComplemento:= '';
  FConta:= '';
  FContaDigito:= '';
  FConvenio:= '';
  FLogradouro:= '';
  FModalidade:= '';
  FNome:= '';
  FNumeroRes:= '';
  FResponEmissao:= tbCliEmite;
  FTelefone:= '';
  FTipoCarteira:= tctSimples;
  FTipoDocumento:= Tradicional;
  FTipoInscricao:= pJuridica;
  FUF:= '';
  FDigitoVerificadorAgenciaConta:= '';
  FIdentDistribuicao := tbClienteDistribui;
  FOperacao := '';
  FPIXChave:= '';
  FPIXTipoChave := tchNenhuma;
end;

procedure TBoletoCedenteConfig.LerIni(const AIni: TCustomIniFile);
var
  LTipoInscricao: integer;
begin

  CNPJCPF:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveCNPJCPF, CNPJCPF );

  LTipoInscricao:= AIni.ReadInteger(CSessaoBoletoCedenteConfig, CChaveTipoInscricao, 1);

  if (TACBrPessoaCedente(LTipoInscricao) >= Low(TACBrPessoaCedente)) and (TACBrPessoaCedente(LTipoInscricao) <= High(TACBrPessoaCedente)) then
     TipoInscricao := TACBrPessoaCedente( LTipoInscricao )
  else
    TipoInscricao := pJuridica;

  Agencia:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveAgencia, Agencia);
  AgenciaDigito:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveAgenciaDigito, AgenciaDigito );
  Bairro:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveBairro, Bairro );
  CaracTitulo:= TACBrCaracTitulo( AIni.ReadInteger(CSessaoBoletoCedenteConfig, CChaveCaracTitulo, integer(CaracTitulo) ));
  CEP:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveCEP, CEP );
  Cidade:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveCidade, Cidade );
  CodigoCedente:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveCodigoCedente, CodigoCedente );
  CodigoTransmissao:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveCodigoTransmissao, CodigoTransmissao );
  Complemento:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveComplemento, Complemento );
  Conta:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveConta, Conta );
  ContaDigito:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveContaDigito, ContaDigito);
  Convenio:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveConvenio, Convenio );
  Logradouro:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveLogradouro, Logradouro );
  Modalidade:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveModalidade, Modalidade );
  Nome:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveNome, Nome );
  NumeroRes:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveNumeroRes, NumeroRes );
  ResponEmissao:= TACBrResponEmissao(AIni.ReadInteger(CSessaoBoletoCedenteConfig, CChaveResponEmissao, integer(ResponEmissao) ));
  Telefone:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveTelefone, Telefone);
  TipoCarteira:= TACBrTipoCarteira( AIni.ReadInteger(CSessaoBoletoCedenteConfig, CChaveTipoCarteira, integer(TipoCarteira) ));
  TipoDocumento:= TACBrTipoDocumento( AIni.ReadInteger(CSessaoBoletoCedenteConfig, CChaveTipoDocumento, integer(TipoDocumento) ));
  UF:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveUF, UF );
  DigitoVerificadorAgenciaConta:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveDigitoVerificadorAgenciaConta, DigitoVerificadorAgenciaConta);
  IdentDistribuicao:= TACBrIdentDistribuicao(AIni.ReadInteger(CSessaoBoletoCedenteConfig, CChaveIdentDistribuicao, integer(FIdentDistribuicao)));
  Operacao:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveOperacao, Operacao);
  PIXChave:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChavePIX, PIXChave);
  PIXTipoChave := TACBrPIXTipoChave(AIni.ReadInteger(CSessaoBoletoCedenteConfig, CTipoChavePix, Integer(PIXTipoChave)));
end;

procedure TBoletoCedenteConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveAgencia, Agencia );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveAgenciaDigito, AgenciaDigito );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveBairro, Bairro );
  AIni.WriteInteger(CSessaoBoletoCedenteConfig, CChaveCaracTitulo, integer(CaracTitulo) );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveCEP, CEP );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveCidade, Cidade );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveCNPJCPF, CNPJCPF );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveCodigoCedente, CodigoCedente );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveCodigoTransmissao, CodigoTransmissao );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveComplemento, Complemento );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveConta, Conta );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveContaDigito,ContaDigito );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveConvenio, Convenio );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveLogradouro, Logradouro );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveModalidade, Modalidade );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveNome, Nome );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveNumeroRes, NumeroRes );
  AIni.WriteInteger(CSessaoBoletoCedenteConfig, CChaveResponEmissao, integer(ResponEmissao) );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveTelefone, Telefone );
  AIni.WriteInteger(CSessaoBoletoCedenteConfig, CChaveTipoCarteira, integer(TipoCarteira) );
  AIni.WriteInteger(CSessaoBoletoCedenteConfig, CChaveTipoDocumento, integer(TipoDocumento) );
  AIni.WriteInteger(CSessaoBoletoCedenteConfig, CChaveTipoInscricao, integer(TipoInscricao) );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveUF, UF  );
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveDigitoVerificadorAgenciaConta, DigitoVerificadorAgenciaConta );
  AIni.WriteInteger(CSessaoBoletoCedenteConfig, CChaveIdentDistribuicao, integer(FIdentDistribuicao));
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChaveOperacao, Operacao);
  AIni.WriteString(CSessaoBoletoCedenteConfig, CChavePIX, PIXChave);
  AIni.WriteInteger(CSessaoBoletoCedenteConfig, CTipoChavePix, integer(PIXTipoChave));
end;

{ TBoletoBancoConfig }
constructor TBoletoBancoConfig.Create;
begin
  FDigito:= 0;
  FLayoutVersaoArquivo:= 0;
  FLayoutVersaoLote:= 0;
  FLocalPagamento:= '';
  FNumero:= 0;
  FNumeroCorrespondente:= 0;
  FOrientacaoBanco:= '';
  FTipoCobranca:= cobNenhum;
end;

procedure TBoletoBancoConfig.LerIni(const AIni: TCustomIniFile);
begin
  Digito:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveDigitoBanco, Digito);
  LayoutVersaoArquivo:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveLayoutVersaoArquivo, LayoutVersaoArquivo);
  LayoutVersaoLote:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveLayoutVersaoLote, LayoutVersaoLote);
  LocalPagamento:= AIni.ReadString(CSessaoBoletoBancoConfig, CChaveLocalPagamento, LocalPagamento);
  Numero:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveNumero, Numero);
  NumeroCorrespondente:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveNumeroCorrespondente, NumeroCorrespondente);
  OrientacaoBanco:= AIni.ReadString(CSessaoBoletoBancoConfig, CChaveOrientacaoBanco, OrientacaoBanco);
  TipoCobranca:= TACBrTipoCobranca( AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveTipoCobranca, integer(TipoCobranca)));
  CasasDecimaisMoraJuros:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveCasasDecimaisMoraJuros, CasasDecimaisMoraJuros);
  //DensidadeGravacao:= AIni.ReadString(CSessaoBoletoBancoConfig, CChaveDensidadeGravacao, DensidadeGravacao);
  CIP:= AIni.ReadString(CSessaoBoletoBancoConfig, CChaveCIP, CIP);
end;

procedure TBoletoBancoConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveDigitoBanco, Digito );
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveLayoutVersaoArquivo, LayoutVersaoArquivo );
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveLayoutVersaoLote, LayoutVersaoLote );
  AIni.WriteString(CSessaoBoletoBancoConfig, CChaveLocalPagamento, LocalPagamento);
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveNumero, Numero );
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveNumeroCorrespondente, NumeroCorrespondente );
  AIni.WriteString(CSessaoBoletoBancoConfig, CChaveOrientacaoBanco, OrientacaoBanco );
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveTipoCobranca, integer(TipoCobranca) );
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveCasasDecimaisMoraJuros, CasasDecimaisMoraJuros);
  //AIni.WriteString(CSessaoBoletoBancoConfig, CChaveDensidadeGravacao, DensidadeGravacao);
  AIni.WriteString(CSessaoBoletoBancoConfig, CChaveCIP, CIP);

end;

{ TBoletoDiretorioConfig }
constructor TBoletoDiretorioConfig.Create;
begin
  FDataArquivo := 0;
  FDataCreditoLanc := 0;
  FDirArqRemessa := '';
  FDirArqRetorno := '';
  FDirHomologacao := False;
  FImprimirMensagemPadrao := True;
  FLayoutRemessa := c240;
  FLeCedenteRetorno := False;
  FNomeArqRemessa := '';
  FNomeArqRetorno := '';
  FNumeroArquivo := 0;
  FRemoveAcentosArqRemessa := False;
  FPrefixArqRemessa:= '';

end;

procedure TBoletoDiretorioConfig.LerIni(const AIni: TCustomIniFile);
begin
  DataArquivo := AIni.ReadDateTime(CSessaoBoletoDiretorioConfig, CChaveDataArquivo, DataArquivo);
  DataCreditoLanc := AIni.ReadDateTime(CSessaoBoletoDiretorioConfig, CChaveDataCreditoLanc, DataCreditoLanc );
  DirArqRemessa := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChaveDirArqRemessa, DirArqRemessa);
  DirArqRetorno := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChaveDirArqRetorno, DirArqRetorno);
  DirHomologacao := AIni.ReadBool(CSessaoBoletoDiretorioConfig, CChaveDirHomologacao, DirHomologacao);
  ImprimirMensagemPadrao := AIni.ReadBool(CSessaoBoletoDiretorioConfig, CChaveImprimirMensagemPadrao, ImprimirMensagemPadrao);
  case AIni.ReadInteger(CSessaoBoletoDiretorioConfig, CChaveLayoutRemessa, integer(LayoutRemessa)) of
    0: LayoutRemessa:= c240;
  else
    LayoutRemessa:= c400;
  end;
  LeCedenteRetorno := AIni.ReadBool(CSessaoBoletoDiretorioConfig, CChaveLeCedenteRetorno, LeCedenteRetorno);
  LerNossoNumeroCompleto:= AIni.ReadBool(CSessaoBoletoDiretorioConfig, CChaveLerNossoNumeroCompleto, LerNossoNumeroCompleto);
  NomeArqRemessa := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChaveNomeArqRemessa, NomeArqRemessa);
  NomeArqRetorno := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChaveNomeArqRetorno, NomeArqRetorno);
  NumeroArquivo := AIni.ReadInteger(CSessaoBoletoDiretorioConfig, CChaveNumeroArquivo, NumeroArquivo);
  RemoveAcentosArqRemessa := AIni.ReadBool(CSessaoBoletoDiretorioConfig, CChaveRemoveAcentosArqRemessa, RemoveAcentosArqRemessa);
  PrefixArqRemessa := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChavePrefixArqRemessa, PrefixArqRemessa);

end;

procedure TBoletoDiretorioConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteDateTime(CSessaoBoletoDiretorioConfig, CChaveDataArquivo, DataArquivo);
  AIni.WriteDateTime(CSessaoBoletoDiretorioConfig, CChaveDataCreditoLanc, DataCreditoLanc);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChaveDirArqRemessa, DirArqRemessa);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChaveDirArqRetorno, DirArqRetorno);
  AIni.WriteBool(CSessaoBoletoDiretorioConfig, CChaveDirHomologacao, DirHomologacao);
  AIni.WriteBool(CSessaoBoletoDiretorioConfig, CChaveImprimirMensagemPadrao, ImprimirMensagemPadrao);
  case integer(LayoutRemessa) of
    0: AIni.WriteInteger(CSessaoBoletoDiretorioConfig, CChaveLayoutRemessa, 1 );
  else
    AIni.WriteInteger(CSessaoBoletoDiretorioConfig, CChaveLayoutRemessa, 0 );
  end;
  AIni.WriteBool(CSessaoBoletoDiretorioConfig, CChaveLeCedenteRetorno, LeCedenteRetorno);
  Aini.WriteBool(CSessaoBoletoDiretorioConfig, CChaveLerNossoNumeroCompleto, LerNossoNumeroCompleto);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChaveNomeArqRemessa, NomeArqRemessa);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChaveNomeArqRetorno, NomeArqRetorno);
  AIni.WriteInteger(CSessaoBoletoDiretorioConfig, CChaveNumeroArquivo, NumeroArquivo);
  AIni.WriteBool(CSessaoBoletoDiretorioConfig, CChaveRemoveAcentosArqRemessa, RemoveAcentosArqRemessa);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChavePrefixArqRemessa, PrefixArqRemessa);

end;

function TLibBoletoConfig.AjustarValor(Tipo: TTipoFuncao; ASessao, AChave,
  AValor: Ansistring): Ansistring;
begin
  Result := '';

  if (ASessao = CSessaoDFe) and (AChave = CChaveDadosPFX) then
  begin
    TACBrLib(Owner).GravarLog(ClassName + '.AjustarValor(' + GetEnumName(TypeInfo(TTipoFuncao), Integer(Tipo)) + ','
                                                          + ASessao + ',' + AChave + ',' +
                                                          IfThen(PrecisaCriptografar(ASessao, AChave),
                                                          StringOfChar('*', Length(AValor)), AValor) +')', logParanoico);
    case Tipo of
      tfGravar: Result := StringToB64Crypt(DecodeBase64(AValor), ChaveCrypt);
      tfLer: Result := EncodeBase64(B64CryptToString(AValor, ChaveCrypt));
    end;

    TACBrLib(Owner).GravarLog(ClassName + '.AjustarValor - Feito Result: ' + Result, logParanoico);
  end
  else
    Result := inherited AjustarValor(Tipo, ASessao, AChave, AValor);
end;

end.

