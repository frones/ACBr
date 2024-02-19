{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
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

unit ACBrLibConsultaCNPJConfig;

interface

uses
  Classes, SysUtils, IniFiles, synachar,
  ACBrLibConfig, ACBrConsultaCNPJ;

type
  { TConsultaCNPJConfig }
  TConsultaCNPJConfig = class
  private
    FEmpresaTipo: String;
    FRazaoSocial: String;
    FAbertura: String;
    FFantasia: String;
    FEndereco: String;
    FNumero: String;
    FComplemento: String;
    FBairro: String;
    FCidade: String;
    FUF: String;
    FCEP: String;
    FSituacao: String;
    FCNAE1: String;
    FCNAE2: String;
    FNaturezaJuridica: String;
    FProvedor : TACBrCNPJProvedorWS;
    FSenha: string;
    FUsuario: string;

    FChaveCrypt: AnsiString;
    function GetSenha: String;
    function Getusuario: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property EmpresaTipo: String read FEmpresaTipo write FEmpresaTipo;
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
    property Abertura: String read FAbertura write FAbertura;
    property Fantasia: String read FFantasia write FFantasia;
    property Endereco: String read FEndereco write FEndereco;
    property Numero: String read FNumero write FNumero;
    property Complemento: String read FComplemento write FComplemento;
    property Bairro: String read FBairro write FBairro;
    property Cidade: String read FCidade write FCidade;
    property UF: String read FUF write FUF;
    property CEP: String read FCEP write FCEP;
    property Situacao: String read FSituacao write FSituacao;
    property CNAE1: String read FCNAE1 write FCNAE1;
    property CNAE2: String read FCNAE2 write FCNAE2;
    property NaturezaJuridica: String read FNaturezaJuridica write FNaturezaJuridica;
    property Provedor : TACBrCNPJProvedorWS read FProvedor write FProvedor;
    property Usuario: String read Getusuario write FUsuario;
    property Senha: String read GetSenha write FSenha;
  end;

  { TLibConsultaCNPJConfig }
  TLibConsultaCNPJConfig = class(TLibConfig)
  private
    FConsultaCNPJConfig: TConsultaCNPJConfig;

  protected
    function AtualizarArquivoConfiguracao: Boolean; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = '');override;
    destructor Destroy; override;

    property ConsultaCNPJConfig: TConsultaCNPJConfig read FConsultaCNPJConfig;
  end;


implementation

uses
  ACBrLibConsultaCNPJBase, ACBrLibConsultaCNPJConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TConsultaCNPJConfig }

function TConsultaCNPJConfig.GetSenha: String;
begin
  Result :=B64CryptToString(FSenha, FChaveCrypt);
end;

function TConsultaCNPJConfig.Getusuario: String;
begin
  Result :=B64CryptToString(FUsuario, FChaveCrypt);
end;

constructor TConsultaCNPJConfig.Create;
begin
  FEmpresaTipo:= '';
  FRazaoSocial:= '';
  FAbertura:= '';
  FFantasia:= '';
  FEndereco:= '';
  FNumero:= '';
  FComplemento:= '';
  FBairro:= '';
  FCidade:= '';
  FUF:= '';
  FCEP:= '';
  FSituacao:= '';
  FCNAE1:= '';
  FCNAE2:= '';
  FNaturezaJuridica:= '';
  FProvedor:=cwsNenhum;
  FUsuario:='';
  FSenha:='';
end;

destructor TConsultaCNPJConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TConsultaCNPJConfig.LerIni(const AIni: TCustomIniFile);
begin
  FRazaoSocial:=       AIni.ReadString(CSessaoConsultaCNPJ, CChaveRazaoSocial, FRazaoSocial);
  FAbertura:=          AIni.ReadString(CSessaoConsultaCNPJ, CChaveAbertura, FAbertura);
  FFantasia:=          AIni.ReadString(CSessaoConsultaCNPJ, CChaveNomeFantasia, FFantasia);
  FEndereco:=          AIni.ReadString(CSessaoConsultaCNPJ, CChaveEndereco, FEndereco);
  FNumero:=            AIni.ReadString(CSessaoConsultaCNPJ, CChaveNumero, FNumero);
  FComplemento:=       AIni.ReadString(CSessaoConsultaCNPJ, CChaveComplemento, FComplemento);
  FBairro:=            Aini.ReadString(CSessaoConsultaCNPJ, CChaveBairo, FBairro);
  FCidade:=            AIni.ReadString(CSessaoConsultaCNPJ, CChaveCidade, FCidade);
  FUF:=                AIni.ReadString(CSessaoConsultaCNPJ, CChaveUF, FUF);
  FCEP:=               AIni.ReadString(CSessaoConsultaCNPJ, CChaveCEP, FCEP);
  FSituacao:=          AIni.ReadString(CSessaoConsultaCNPJ, CChaveSituacao, FSituacao);
  FCNAE1:=             AIni.ReadString(CSessaoConsultaCNPJ, CChaveCNAE1, FCNAE1);
  FCNAE2:=             AIni.ReadString(CSessaoConsultaCNPJ, CChaveCNAE2, FCNAE2);
  FNaturezaJuridica:=  AIni.ReadString(CSessaoConsultaCNPJ, CChaveNaturezaJuridica, FNaturezaJuridica);
  FProvedor:=          TACBrCNPJProvedorWS(AIni.ReadInteger(CSessaoConsultaCNPJ, CChaveProvedor, integer(FProvedor)));
  FUsuario:=           AIni.ReadString(CSessaoConsultaCNPJ, CChaveUsuario, FUsuario);
  FSenha:=             AIni.ReadString(CSessaoConsultaCNPJ, CChaveSenha, FSenha);
end;

procedure TConsultaCNPJConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveRazaoSocial, FRazaoSocial);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveAbertura, FAbertura);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveNomeFantasia, FFantasia);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveEndereco, FEndereco);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveNumero, FNumero);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveComplemento, FComplemento);
  Aini.WriteString(CSessaoConsultaCNPJ, CChaveBairo, FBairro);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveCidade, FCidade);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveUF, FUF);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveCEP, FCEP);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveSituacao, FSituacao);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveCNAE1, FCNAE1);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveCNAE2, FCNAE2);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveNaturezaJuridica, FNaturezaJuridica);
  AIni.WriteInteger(CSessaoConsultaCNPJ,CChaveProvedor,integer(FProvedor));
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveUsuario, FUsuario);
  AIni.WriteString(CSessaoConsultaCNPJ, CChaveSenha, FSenha);
end;

{ TLibConsultaCNPJConfig }

constructor TLibConsultaCNPJConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);
  FConsultaCNPJConfig := TConsultaCNPJConfig.Create;
end;

destructor TLibConsultaCNPJConfig.Destroy;
begin
  FConsultaCNPJConfig.Free;
  inherited Destroy;
end;

function TLibConsultaCNPJConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibConsultaCNPJNome, '0');
  Result := (CompareVersions(CLibConsultaCNPJVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibConsultaCNPJConfig.INIParaClasse;
begin
  inherited INIParaClasse;
  FConsultaCNPJConfig.LerIni(Ini);
end;

procedure TLibConsultaCNPJConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;
  Ini.WriteString(CSessaoVersao, CLibConsultaCNPJNome, CLibConsultaCNPJVersao);
  FConsultaCNPJConfig.GravarIni(Ini);
end;

procedure TLibConsultaCNPJConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
  TACBrLibConsultaCNPJ(Owner).ConsultaCNPJDM.AplicarConfiguracoes;
end;

end.

