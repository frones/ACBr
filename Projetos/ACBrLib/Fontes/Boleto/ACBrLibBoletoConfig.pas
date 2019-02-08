{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: José M. S. Junior                               }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}


{$I ACBr.inc}

unit ACBrLibBoletoConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrBoleto, ACBrLibConfig;

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
    FNomeArqRemessa: String;
    FNomeArqRetorno: String;
    FNumeroArquivo: Integer;

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
    property NomeArqRemessa: String read FNomeArqRemessa write FNomeArqRemessa;
    property NomeArqRetorno: String read FNomeArqRetorno write FNomeArqRetorno;
    property NumeroArquivo: Integer read FNumeroArquivo write FNumeroArquivo;

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
    FTamanhoMaximoNossoNumero: Integer;
    FTipoCobranca: TACBrTipoCobranca;

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
    property TamanhoMaximoNossoNumero: Integer read FTamanhoMaximoNossoNumero write FTamanhoMaximoNossoNumero;
    property TipoCobranca: TACBrTipoCobranca read FTipoCobranca write FTipoCobranca;

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

  end;

  { TBoletoFCFortesConfig }

  TBoletoFCFortesConfig = class
  private
    FDirLogo: String;
    FFiltro: TACBrBoletoFCFiltro;
    FLayout: TACBrBolLayOut;
    FMostrarPreview: Boolean;
    FMostrarProgresso: Boolean;
    FMostrarSetup: Boolean;
    FNomeArquivo: String;
    FNumeroCopias: Integer;
    FPrinterName: String;
    FSoftwareHouse: String;

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

  protected
    function AtualizarArquivoConfiguracao: Boolean; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: string = ''; AChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property BoletoDiretorioConfig: TBoletoDiretorioConfig read FBoletoDiretorioConfig;
    property BoletoBancoConfig: TBoletoBancoConfig read FBoletoBancoConfig;
    property BoletoCedenteConfig: TBoletoCedenteConfig read FBoletoCedenteConfig;
    property BoletoFCFortesConfig: TBoletoFCFortesConfig read FBoletoFCFortesConfig;
    property BoletoConfig: TBoletoConfig read FBoletoConfig;

  end;


implementation

uses
  ACBrLibBoletoConsts, ACBrLibComum,
  ACBrUtil, ACBrConsts, ACBrLibConsts, ACBrLibBoletoClass;

{ TLibBoletoConfig }

function TLibBoletoConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibBoletoNome, '0');
  Result := (CompareVersions(CLibBoletoVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibBoletoConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FBoletoDiretorioConfig.LerIni(Ini);
  FBoletoBancoConfig.LerIni(Ini);
  FBoletoCedenteConfig.LerIni(Ini);
  FBoletoFCFortesConfig.LerIni(Ini);
  FBoletoConfig.LerIni(Ini);

end;

procedure TLibBoletoConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibBoletoNome, CLibBoletoVersao);

  FBoletoDiretorioConfig.GravarIni(Ini);
  FBoletoBancoConfig.GravarIni(Ini);
  FBoletoCedenteConfig.GravarIni(Ini);
  FBoletoFCFortesConfig.GravarIni(Ini);
  FBoletoConfig.GravarIni(Ini);

end;

procedure TLibBoletoConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibBoleto(Owner).BoletoDM.AplicarConfiguracoes;
end;

procedure TLibBoletoConfig.Travar;
begin
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
end;

destructor TLibBoletoConfig.Destroy;
begin
  FBoletoDiretorioConfig.Free;
  FBoletoBancoConfig.Free;
  FBoletoCedenteConfig.Free;
  FBoletoFCFortesConfig.Free;
  FBoletoConfig.Free;

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

end;

procedure TBoletoCedenteConfig.LerIni(const AIni: TCustomIniFile);
begin
  Agencia:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveAgencia, Agencia);
  AgenciaDigito:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveAgenciaDigito, AgenciaDigito );
  Bairro:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveBairro, Bairro );
  CaracTitulo:= TACBrCaracTitulo( AIni.ReadInteger(CSessaoBoletoCedenteConfig, CChaveCaracTitulo, integer(CaracTitulo) ));
  CEP:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveCEP, CEP );
  Cidade:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveCidade, Cidade );
  CNPJCPF:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveCNPJCPF, CNPJCPF );
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
  TipoInscricao:= TACBrPessoaCedente( AIni.ReadInteger(CSessaoBoletoCedenteConfig, CChaveTipoInscricao, integer(TipoInscricao) ));
  UF:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveUF, UF );
  DigitoVerificadorAgenciaConta:= AIni.ReadString(CSessaoBoletoCedenteConfig, CChaveDigitoVerificadorAgenciaConta, DigitoVerificadorAgenciaConta );

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
  FTamanhoMaximoNossoNumero:= 0;
  FTipoCobranca:= cobNenhum;
end;

procedure TBoletoBancoConfig.LerIni(const AIni: TCustomIniFile);
begin
  Digito:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveDigitoBanco, Digito);
  LayoutVersaoArquivo:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveLayoutVersaoArquivo, LayoutVersaoArquivo );
  LayoutVersaoLote:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveLayoutVersaoLote, LayoutVersaoLote);
  LocalPagamento:= AIni.ReadString(CSessaoBoletoBancoConfig, CChaveLocalPagamento, LocalPagamento );
  Numero:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveNumero, Numero );
  NumeroCorrespondente:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveNumeroCorrespondente, NumeroCorrespondente );
  OrientacaoBanco:= AIni.ReadString(CSessaoBoletoBancoConfig, CChaveOrientacaoBanco, OrientacaoBanco );
  TamanhoMaximoNossoNumero:= AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveTamanhoMaximoNossoNumero, TamanhoMaximoNossoNumero );
  TipoCobranca:= TACBrTipoCobranca( AIni.ReadInteger(CSessaoBoletoBancoConfig, CChaveTipoCobranca, integer(TipoCobranca) ));

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
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveTamanhoMaximoNossoNumero , TamanhoMaximoNossoNumero );
  AIni.WriteInteger(CSessaoBoletoBancoConfig, CChaveTipoCobranca, integer(TipoCobranca) );

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

end;

procedure TBoletoDiretorioConfig.LerIni(const AIni: TCustomIniFile);
begin
  DataArquivo := AIni.ReadDateTime(CSessaoBoletoDiretorioConfig, CChaveDataArquivo, DataArquivo);
  DataCreditoLanc := AIni.ReadDateTime(CSessaoBoletoDiretorioConfig, CChaveDataCreditoLanc, DataCreditoLanc );
  DirArqRemessa := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChaveDirArqRemessa, DirArqRemessa);
  DirArqRetorno := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChaveDirArqRetorno, DirArqRetorno);
  DirHomologacao := AIni.ReadBool(CSessaoBoletoDiretorioConfig, CChaveDirHomologacao, DirHomologacao);
  ImprimirMensagemPadrao := AIni.ReadBool(CSessaoBoletoDiretorioConfig, CChaveImprimirMensagemPadrao, ImprimirMensagemPadrao);
  LayoutRemessa := TACBrLayoutRemessa(AIni.ReadInteger(CSessaoBoletoDiretorioConfig, CChaveLayoutRemessa, integer(LayoutRemessa)));
  LeCedenteRetorno := AIni.ReadBool(CSessaoBoletoDiretorioConfig, CChaveLeCedenteRetorno, LeCedenteRetorno);
  NomeArqRemessa := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChaveNomeArqRemessa, NomeArqRemessa);
  NomeArqRetorno := AIni.ReadString(CSessaoBoletoDiretorioConfig, CChaveNomeArqRetorno, NomeArqRetorno);
  NumeroArquivo := AIni.ReadInteger(CSessaoBoletoDiretorioConfig, CChaveNumeroArquivo, NumeroArquivo);

end;

procedure TBoletoDiretorioConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteDateTime(CSessaoBoletoDiretorioConfig, CChaveDataArquivo, DataArquivo);
  AIni.WriteDateTime(CSessaoBoletoDiretorioConfig, CChaveDataCreditoLanc, DataCreditoLanc);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChaveDirArqRemessa, DirArqRemessa);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChaveDirArqRetorno, DirArqRetorno);
  AIni.WriteBool(CSessaoBoletoDiretorioConfig, CChaveDirHomologacao, DirHomologacao);
  AIni.WriteBool(CSessaoBoletoDiretorioConfig, CChaveImprimirMensagemPadrao, ImprimirMensagemPadrao);
  AIni.WriteInteger(CSessaoBoletoDiretorioConfig, CChaveLayoutRemessa, integer(LayoutRemessa) );
  AIni.WriteBool(CSessaoBoletoDiretorioConfig, CChaveLeCedenteRetorno, LeCedenteRetorno);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChaveNomeArqRemessa, NomeArqRemessa);
  AIni.WriteString(CSessaoBoletoDiretorioConfig, CChaveNomeArqRetorno, NomeArqRetorno);
  AIni.WriteInteger(CSessaoBoletoDiretorioConfig, CChaveNumeroArquivo, NumeroArquivo);

end;

end.

