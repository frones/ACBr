{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrNFSeLibConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrLibComum, ACBrLibConfig, DFeReportConfig,
  ACBrNFSeXDANFSeRLClass, ACBrNFSeXConversao, ACBrNFSeXConfiguracoes;

type
   { TDANFSeReportConfig }
  TDANFSeReportConfig = class(TDFeReportConfig<TACBrNFSeXDANFSeRL>)
  private
    FPrestadorLogo: String;
    FPrefeitura: String;
    FPrestadorRazaoSocial: String;
    FPrestadorNomeFantasia: String;
    FPrestadorEndereco: String;
    FPrestadorComplemento: String;
    FPrestadorFone: String;
    FPrestadorMunicipio: String;
    FOutrasInformacaoesImp: String;
    FPrestadorInscMunicipal: String;
    FTomadorInscEstadual: String;
    FTomadorInscMunicipal: String;
    FTomadorFone: String;
    FTomadorEndereco: String;
    FTomadorComplemento: String;
    FTomadorEmail: String;
    FPrestadorEMail: String;
    FPrestadorCNPJ: String;
    FFormatarNumeroDocumentoNFSe: Boolean;
    FPrestadorUF: String;
    FAtividade: String;
    FNFSeCancelada: boolean;
    FImprimeCanhoto: Boolean;
    FTipoDANFSE: TTipoDANFSE;
    FTamanhoFonte: Integer;
    FProducao: TnfseSimNao;
    FDetalharServico : Boolean;

  protected
    procedure LerIniChild(const AIni: TCustomIniFile); override;
    procedure GravarIniChild(const AIni: TCustomIniFile); override;
    procedure ApplyChild(const DFeReport: TACBrNFSeXDANFSeRL; const Lib: TACBrLib); override;
    procedure DefinirValoresPadroesChild; override;

  public
    constructor Create;

    property PrestadorLogo: String read FPrestadorLogo write FPrestadorLogo;
    property Prefeitura: String read FPrefeitura write FPrefeitura;
    property PrestadorRazaoSocial: String read FPrestadorRazaoSocial write FPrestadorRazaoSocial;
    property PrestadorNomeFantasia: String read FPrestadorNomeFantasia write FPrestadorNomeFantasia;
    property PrestadorUF: String read FPrestadorUF write FPrestadorUF;
    property PrestadorEndereco: String read FPrestadorEndereco write FPrestadorEndereco;
    property PrestadorComplemento: String read FPrestadorComplemento write FPrestadorComplemento;
    property PrestadorFone: String read FPrestadorFone write FPrestadorFone;
    property PrestadorMunicipio: String read FPrestadorMunicipio write FPrestadorMunicipio;
    property OutrasInformacaoesImp: String read FOutrasInformacaoesImp write FOutrasInformacaoesImp;
    property PrestadorInscMunicipal: String read FPrestadorInscMunicipal write FPrestadorInscMunicipal;
    property PrestadorEMail: String read FPrestadorEMail write FPrestadorEMail;
    property PrestadorCNPJ: String read FPrestadorCNPJ write FPrestadorCNPJ;
    property TomadorInscEstadual: String read FTomadorInscEstadual write FTomadorInscEstadual;
    property TomadorInscMunicipal: String read FTomadorInscMunicipal write FTomadorInscMunicipal;
    property TomadorFone: String read FTomadorFone write FTomadorFone;
    property TomadorEndereco: String read FTomadorEndereco write FTomadorEndereco;
    property TomadorComplemento: String read FTomadorComplemento write FTomadorComplemento;
    property TomadorEmail: String read FTomadorEmail write FTomadorEmail;
    property Atividade: String read FAtividade write FAtividade;
    property Cancelada: Boolean read FNFSeCancelada write FNFSeCancelada;
    property ImprimeCanhoto: Boolean read FImprimeCanhoto write FImprimeCanhoto default False;
    property TipoDANFSE: TTipoDANFSE read FTipoDANFSE   write FTipoDANFSE default tpPadrao;
    property TamanhoFonte: Integer   read FTamanhoFonte write FTamanhoFonte;
    property FormatarNumeroDocumentoNFSe: Boolean read FFormatarNumeroDocumentoNFSe write FFormatarNumeroDocumentoNFSe;
    property Producao: TnfseSimNao   read FProducao     write FProducao;
    property DetalharServico: Boolean read FDetalharServico write FDetalharServico default False;

  end;

  { TLibNFSeConfig }
  TLibNFSeConfig = class(TLibConfig)
  private
    FNFSeConfig: TConfiguracoesNFSe;
    FDANFSeConfig: TDANFSeReportConfig;

  protected
    procedure Travar; override;
    procedure Destravar; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property NFSe: TConfiguracoesNFSe read FNFSeConfig;
    property DANFSe: TDANFSeReportConfig read FDANFSeConfig;
  end;

implementation

uses
  ACBrLibNFSeBase, ACBrLibNFSeConsts;

{ TDANFSeReportConfig }
constructor TDANFSeReportConfig.Create;
begin
  inherited Create(CSessaoDANFSE);
end;

procedure TDANFSeReportConfig.LerIniChild(const AIni: TCustomIniFile);
begin
  FPrestadorLogo := AIni.ReadString(CSessaoDANFSE, CChavePLogo, FPrestadorLogo);
  FPrestadorRazaoSocial := AIni.ReadString(CSessaoDANFSE, CChavePRazaoSocial, FPrestadorRazaoSocial);
  FPrestadorNomeFantasia := AIni.ReadString(CSessaoDANFSE, CChavePNomeFantasia, FPrestadorNomeFantasia);
  FPrestadorEndereco := AIni.ReadString(CSessaoDANFSE, CChavePEndereco, FPrestadorEndereco);
  FPrestadorComplemento := AIni.ReadString(CSessaoDANFSE, CChavePComplemento, FPrestadorComplemento);
  FPrestadorFone := AIni.ReadString(CSessaoDANFSE, CChavePFone, FPrestadorFone);
  FPrestadorMunicipio := AIni.ReadString(CSessaoDANFSE, CChavePMunicipio, FPrestadorMunicipio);
  FPrestadorInscMunicipal := AIni.ReadString(CSessaoDANFSE, CChavePIM, FPrestadorInscMunicipal);
  FPrestadorEMail := AIni.ReadString(CSessaoDANFSE, CChavePEMail, FPrestadorEMail);
  FPrestadorUF := AIni.ReadString(CSessaoDANFSE, CChavePUF, FPrestadorUF);

  FTomadorInscEstadual := AIni.ReadString(CSessaoDANFSE, CChaveTIE, FTomadorInscEstadual);
  FTomadorInscMunicipal := AIni.ReadString(CSessaoDANFSE, CChaveTIM, FTomadorInscMunicipal);
  FTomadorFone := AIni.ReadString(CSessaoDANFSE, CChaveTFone, FTomadorFone);
  FTomadorEndereco := AIni.ReadString(CSessaoDANFSE, CChavePUF, CChaveTEndereco);
  FTomadorComplemento := AIni.ReadString(CSessaoDANFSE, CChaveTComplemento, FTomadorComplemento);
  FTomadorEmail:= AIni.ReadString(CSessaoDANFSE, CChaveTEMail, FTomadorEmail);

  FPrefeitura := AIni.ReadString(CSessaoDANFSE, CChavePrefeitura, FPrefeitura);
  FTamanhoFonte := AIni.ReadInteger(CSessaoDANFSE, CChaveTamanhoFonte, FTamanhoFonte);
  FOutrasInformacaoesImp := AIni.ReadString(CSessaoDANFSE, CChaveOutrasInformacaoesImp, OutrasInformacaoesImp);
  FAtividade := AIni.ReadString(CSessaoDANFSE, CChaveAtividade, FAtividade);
  FFormatarNumeroDocumentoNFSe := AIni.ReadBool(CSessaoDANFSE, CChaveFmtNroNFSe, FFormatarNumeroDocumentoNFSe);
  FNFSeCancelada := AIni.ReadBool(CSessaoDANFSE, CChaveNFSeCancelada, FNFSeCancelada);
  FDetalharServico := AIni.ReadBool(CSessaoDANFSE, CChaveDetalharServico, FDetalharServico);
  FProducao := TnfseSimNao(AIni.ReadInteger(CSessaoDANFSE, CChaveProducao, Integer(FProducao)));
end;

procedure TDANFSeReportConfig.GravarIniChild(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoDANFSE, CChavePLogo, FPrestadorLogo);
  AIni.WriteString(CSessaoDANFSE, CChavePRazaoSocial, FPrestadorRazaoSocial);
  AIni.WriteString(CSessaoDANFSE, CChavePNomeFantasia, FPrestadorNomeFantasia);
  AIni.WriteString(CSessaoDANFSE, CChavePEndereco, FPrestadorEndereco);
  AIni.WriteString(CSessaoDANFSE, CChavePComplemento, FPrestadorComplemento);
  AIni.WriteString(CSessaoDANFSE, CChavePFone, FPrestadorFone);
  AIni.WriteString(CSessaoDANFSE, CChavePMunicipio, FPrestadorMunicipio);
  AIni.WriteString(CSessaoDANFSE, CChavePIM, FPrestadorInscMunicipal);
  AIni.WriteString(CSessaoDANFSE, CChavePEMail, FPrestadorEMail);
  AIni.WriteString(CSessaoDANFSE, CChavePUF, FPrestadorUF);

  AIni.WriteString(CSessaoDANFSE, CChaveTIE, FTomadorInscEstadual);
  AIni.WriteString(CSessaoDANFSE, CChaveTIM, FTomadorInscMunicipal);
  AIni.WriteString(CSessaoDANFSE, CChaveTFone, FTomadorFone);
  AIni.WriteString(CSessaoDANFSE, CChavePUF, CChaveTEndereco);
  AIni.WriteString(CSessaoDANFSE, CChaveTComplemento, FTomadorComplemento);
  AIni.WriteString(CSessaoDANFSE, CChaveTEMail, FTomadorEmail);

  AIni.WriteString(CSessaoDANFSE, CChavePrefeitura, FPrefeitura);
  AIni.WriteInteger(CSessaoDANFSE, CChaveTamanhoFonte, FTamanhoFonte);
  AIni.WriteString(CSessaoDANFSE, CChaveOutrasInformacaoesImp, OutrasInformacaoesImp);
  AIni.WriteString(CSessaoDANFSE, CChaveAtividade, FAtividade);
  AIni.WriteBool(CSessaoDANFSE, CChaveFmtNroNFSe, FFormatarNumeroDocumentoNFSe);
  AIni.WriteBool(CSessaoDANFSE, CChaveNFSeCancelada, FNFSeCancelada);
  AIni.WriteBool(CSessaoDANFSE, CChaveDetalharServico, FDetalharServico);
  AIni.WriteInteger(CSessaoDANFSE, CChaveProducao, Integer(FProducao));
end;

procedure TDANFSeReportConfig.ApplyChild(const DFeReport: TACBrNFSeXDANFSeRL; const Lib: TACBrLib);
begin
  with DFeReport do
  begin
    Prestador.Logo := FPrestadorLogo;
    Prestador.RazaoSocial := FPrestadorRazaoSocial;
    Prestador.NomeFantasia := FPrestadorNomeFantasia;
    Prestador.Endereco := FPrestadorEndereco;
    Prestador.Complemento := FPrestadorComplemento;
    Prestador.Fone := FPrestadorFone;
    Prestador.Municipio := FPrestadorMunicipio;
    Prestador.EMail := FPrestadorEMail;
    Prestador.UF := FPrestadorUF;
    Prestador.InscricaoMunicipal := FPrestadorInscMunicipal;

    Tomador.InscricaoEstadual := FTomadorInscEstadual;
    Tomador.InscricaoMunicipal := FTomadorInscMunicipal;
    Tomador.Fone := FTomadorFone;
    Tomador.Endereco := FTomadorEndereco;
    Tomador.Complemento := FTomadorComplemento;
    Tomador.Email := FTomadorEmail;

    Prefeitura := FPrefeitura;
    TamanhoFonte := FTamanhoFonte;
    OutrasInformacaoesImp := FOutrasInformacaoesImp;
    Atividade := FAtividade;
    FormatarNumeroDocumentoNFSe := FFormatarNumeroDocumentoNFSe;
    Cancelada := FNFSeCancelada;
    DetalharServico := FDetalharServico;
    Producao := FProducao;
  end;
end;

procedure TDANFSeReportConfig.DefinirValoresPadroesChild;
begin
  FPrestadorLogo := '';
  FPrefeitura := '';
  FPrestadorRazaoSocial := '';
  FPrestadorNomeFantasia := '';
  FPrestadorEndereco := '';
  FPrestadorComplemento := '';
  FPrestadorFone := '';
  FPrestadorMunicipio := '';
  FTamanhoFonte := 6;
  FOutrasInformacaoesImp := '';
  FPrestadorInscMunicipal := '';
  FPrestadorEMail := '';
  FPrestadorUF := '';
  FTomadorInscEstadual := '';
  FTomadorInscMunicipal := '';
  FAtividade := '';
  FTomadorFone := '';
  FTomadorEndereco := '';
  FTomadorComplemento := '';
  FTomadorEmail := '';
  FFormatarNumeroDocumentoNFSe := True;
  FNFSeCancelada := False;
  FDetalharServico := False;
end;

{ TLibNFSeConfig }

constructor TLibNFSeConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FNFSeConfig := TConfiguracoesNFSe.Create(nil);
  FNFSeConfig.ChaveCryptINI := AChaveCrypt;

  FDANFSeConfig := TDANFSeReportConfig.Create;
end;

destructor TLibNFSeConfig.Destroy;
begin
  FNFSeConfig.Free;
  FDANFSeConfig.Free;

  inherited Destroy;
end;

procedure TLibNFSeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FNFSeConfig.ChaveCryptINI := ChaveCrypt;
  FNFSeConfig.LerIni(Ini);
  FDANFSeConfig.LerIni(Ini);
end;

procedure TLibNFSeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FNFSeConfig.ChaveCryptINI := ChaveCrypt;
  FNFSeConfig.GravarIni(Ini);
  FDANFSeConfig.GravarIni(Ini);
end;

procedure TLibNFSeConfig.ClasseParaComponentes;
begin
  FNFSeConfig.ChaveCryptINI := ChaveCrypt;

  if Assigned(Owner) then
    TACBrLibNFSe(Owner).NFSeDM.AplicarConfiguracoes;
end;

procedure TLibNFSeConfig.Travar;
begin
  if Assigned(Owner) then
    TACBrLibNFSe(Owner).NFSeDM.Travar;
end;

procedure TLibNFSeConfig.Destravar;
begin
  if Assigned(Owner) then
    TACBrLibNFSe(Owner).NFSeDM.Destravar;
end;

end.

