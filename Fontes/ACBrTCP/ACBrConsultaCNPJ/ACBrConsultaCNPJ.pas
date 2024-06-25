{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Victor H Gonzales - Pandaaa                    }
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

unit ACBrConsultaCNPJ;

interface

uses
  SysUtils,
  Classes,
  types,
  IniFiles,
  ACBrBase,
  ACBrSocket,
  ACBrIBGE,
  ACBrConsultaCNPJ.WS;

type
  TACBrOnSolicitaCaptchaHTTP = procedure( var AHtml : String ) of object ;
  EACBrConsultaCNPJException = class ( Exception );
  TACBrCNPJProvedorWS = (cwsNenhum, cwsBrasilAPI, cwsReceitaWS, cwsCNPJWS);

  { TACBrConsultaCNPJ }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrConsultaCNPJ = class(TACBrHTTP)
  protected
    FACBrIBGE: TACBrIBGE;
    FNaturezaJuridica : String ;
    //FViewState: String;
    FEmpresaTipo: String;
    FAbertura: TDateTime;
    FRazaoSocial: String;
    FFantasia: String;
    FPorte: String;
    FCNAE1: String;
    FCNAE2: TStringList;
    FEndereco: String;
    FNumero: String;
    FComplemento: String;
    FCEP: String;
    FBairro: String;
    FCidade: String;
    FUF: String;
    FSituacao: String;
    FSituacaoEspecial : String;
    FCNPJ: String;
    FDataSituacao: TDateTime;
    FDataSituacaoEspecial : TDateTime;
    FEndEletronico: String;
    FTelefone: String;
    FEFR: string;  //ENTE FEDERATIVO RESPONSÁVEL (EFR)
    FMotivoSituacaoCad: string;
    FPesquisarIBGE: Boolean;
    FCodigoIBGE: String;
    FIniServicos: string;
    FResourceName: String;
    FParams: TStrings;
    FOnSolicitarCaptcha: TACBrOnSolicitaCaptchaHTTP;
    FProvedor : TACBrCNPJProvedorWS;
    FUsuario: String;
    FSenha: String;
    FInscricaoEstadual : String;
    FDefasagemMaximo : Integer;
    FProxyHost : string;
    FProxyPort : string;
    FProxyUser : string;
    FProxyPass : string;
    FCapitalSocial : Double;
    function GetIBGE_UF : String ;
    function GetIniServicos: String;
    procedure ParserWS(const AACBrConsultaCNPJWSResposta : TACBrConsultaCNPJWSResposta);
  public
    procedure Captcha(const Stream: TStream); deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS}'Metodo sem utilidade atualmente.'{$ENDIF};
    function Consulta(const ACNPJ: String; const ACaptcha: String = ''; const ARemoverEspacosDuplos: Boolean = False): Boolean;
    procedure Clear;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnSolicitarCaptcha: TACBrOnSolicitaCaptchaHTTP read FOnSolicitarCaptcha write FOnSolicitarCaptcha;

    property CNPJ: String Read FCNPJ Write FCNPJ;
    property EmpresaTipo: String Read FEmpresaTipo;
    property Abertura: TDateTime Read FAbertura;
    property RazaoSocial: String Read FRazaoSocial;
    property Fantasia: String Read FFantasia;
    property Porte: String read FPorte;
    property CNAE1: String Read FCNAE1;
    property CNAE2: TStringList Read FCNAE2;
    property Endereco: String Read FEndereco;
    property Numero: String Read FNumero;
    property Complemento: String Read FComplemento;
    property CEP: String Read FCEP;
    property Bairro: String Read FBairro;
    property Cidade: String Read FCidade;
    property UF: String Read FUF;
    property Situacao: String Read FSituacao;
    property SituacaoEspecial: String Read FSituacaoEspecial;
    property DataSituacao: TDateTime Read FDataSituacao;
    property DataSituacaoEspecial : TDatetime Read FDataSituacaoEspecial;
    property NaturezaJuridica: String Read FNaturezaJuridica;
    property EndEletronico: string read FEndEletronico;
    property Telefone: String read FTelefone;
    property EFR: string read FEFR;
    property MotivoSituacaoCad: string read FMotivoSituacaoCad;
    property IBGE_Municipio  : String read FCodigoIBGE;
    property IBGE_UF         : String read GetIBGE_UF ;
    property PesquisarIBGE: Boolean read FPesquisarIBGE write FPesquisarIBGE;
    property IniServicos : string read GetIniServicos write FIniServicos;
    property Provedor : TACBrCNPJProvedorWS read FProvedor write FProvedor default cwsNenhum;
    property Usuario: String read FUsuario write FUsuario;
    property Senha: String read FSenha write FSenha;
    property InscricaoEstadual: String read FInscricaoEstadual;
    property DefasagemMaximo: Integer read FDefasagemMaximo write FDefasagemMaximo default 999;
    property ProxyHost: string read FProxyHost write FProxyHost;
    property ProxyPort: string read FProxyPort write FProxyPort;
    property ProxyUser: string read FProxyUser write FProxyUser;
    property ProxyPass: string read FProxyPass write FProxyPass;
    property CapitalSocial: Double read FCapitalSocial write FCapitalSocial;
  end;

implementation

uses
  strutils,
  blcksock, synautil,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.XMLHTML,
  ACBrValidador,
  ACBrUtil.FilesIO,
  ACBrConsultaCNPJ.WS.ReceitaWS,
  ACBrConsultaCNPJ.WS.BrasilAPI,
  ACBrConsultaCNPJ.WS.CNPJWS;

procedure TACBrConsultaCNPJ.Captcha(const Stream: TStream);
begin
  if Self.Provedor = cwsNenhum then
   raise EACBrConsultaCNPJException.Create('Utilize comunicação via WebServices.');
end;

procedure TACBrConsultaCNPJ.ParserWS(const AACBrConsultaCNPJWSResposta : TACBrConsultaCNPJWSResposta);
begin
  FNaturezaJuridica     := AACBrConsultaCNPJWSResposta.NaturezaJuridica;
  FEmpresaTipo          := AACBrConsultaCNPJWSResposta.EmpresaTipo;
  FAbertura             := AACBrConsultaCNPJWSResposta.Abertura;
  FRazaoSocial          := AACBrConsultaCNPJWSResposta.RazaoSocial;
  FFantasia             := AACBrConsultaCNPJWSResposta.Fantasia;
  FPorte                := AACBrConsultaCNPJWSResposta.Porte;
  FCNAE1                := AACBrConsultaCNPJWSResposta.CNAE1;
  FCNAE2.Text           := AACBrConsultaCNPJWSResposta.CNAE2.Text;
  FEndereco             := AACBrConsultaCNPJWSResposta.Endereco;
  FNumero               := AACBrConsultaCNPJWSResposta.Numero;
  FComplemento          := AACBrConsultaCNPJWSResposta.Complemento;
  FCEP                  := AACBrConsultaCNPJWSResposta.CEP;
  FBairro               := AACBrConsultaCNPJWSResposta.Bairro;
  FCidade               := AACBrConsultaCNPJWSResposta.Cidade;
  FUF                   := AACBrConsultaCNPJWSResposta.UF;
  FSituacao             := AACBrConsultaCNPJWSResposta.Situacao;
  FSituacaoEspecial     := AACBrConsultaCNPJWSResposta.SituacaoEspecial;
  FCNPJ                 := AACBrConsultaCNPJWSResposta.CNPJ;
  FDataSituacao         := AACBrConsultaCNPJWSResposta.DataSituacao;
  FDataSituacaoEspecial := AACBrConsultaCNPJWSResposta.DataSituacaoEspecial;
  FEndEletronico        := AACBrConsultaCNPJWSResposta.EndEletronico;
  FTelefone             := AACBrConsultaCNPJWSResposta.Telefone;
  FEFR                  := AACBrConsultaCNPJWSResposta.EFR;
  FMotivoSituacaoCad    := AACBrConsultaCNPJWSResposta.MotivoSituacaoCad;
  FCodigoIBGE           := AACBrConsultaCNPJWSResposta.CodigoIBGE;
  FInscricaoEstadual    := AACBrConsultaCNPJWSResposta.InscricaoEstadual;
  FCapitalSocial        := AACBrConsultaCNPJWSResposta.CapitalSocial;
end;

function TACBrConsultaCNPJ.Consulta(const ACNPJ: String; const ACaptcha: String; const ARemoverEspacosDuplos: Boolean): Boolean;
var
  LErro : String;
  LACBrConsultaCNPJWS : TACBrConsultaCNPJWS;
begin
  LErro := ValidarCNPJ( ACNPJ ) ;
  if LErro <> '' then
     raise EACBrConsultaCNPJException.Create(LErro);

  if Self.Provedor <> cwsNenhum then
  begin
    case Self.Provedor of
      cwsReceitaWS : LACBrConsultaCNPJWS := TACBrConsultaCNPJWSReceitaWS.Create( ACNPJ, Self.Usuario, Self.Senha, Self.DefasagemMaximo );
      cwsBrasilAPI : LACBrConsultaCNPJWS := TACBrConsultaCNPJWSBrasilAPI.Create( ACNPJ );
      cwsCNPJWS    : LACBrConsultaCNPJWS := TACBrConsultaCNPJWSCNPJWS.Create( ACNPJ, Self.Usuario, Self.Senha );
    end;

    try
        LACBrConsultaCNPJWS.FProxyHost := ProxyHost;
        LACBrConsultaCNPJWS.FProxyPort := FProxyPort;
        LACBrConsultaCNPJWS.FProxyUser := FProxyUser;
        LACBrConsultaCNPJWS.FProxyPass := FProxyPass;

        Result := LACBrConsultaCNPJWS.Executar;
        ParserWS(LACBrConsultaCNPJWS.FResposta);
        Exit;

    finally
      LACBrConsultaCNPJWS.Free;
    end;
  end;
end;

constructor TACBrConsultaCNPJ.Create(AOwner: TComponent);
begin
  inherited;
  FCNAE2 := TStringList.Create;
  FPesquisarIBGE := False;
  fACBrIBGE := TACBrIBGE.Create(nil);
  FACBrIBGE.IgnorarCaixaEAcentos := True;
  HTTPSend.Sock.SSL.SSLType := LT_TLSv1;
  FResourceName := 'ACBrConsultaCNPJServicos';
  FParams := TStringList.Create;
  FProvedor := cwsNenhum;
  FDefasagemMaximo := 999;
end;

destructor TACBrConsultaCNPJ.Destroy;
begin
  fACBrIBGE.Free;
  FCNAE2.Free;
  FParams.Free;
  inherited;
end;

procedure TACBrConsultaCNPJ.Clear;
begin
  FNaturezaJuridica := '';
  FEmpresaTipo      := '';
  FAbertura         := 0;
  FRazaoSocial      := '';
  FFantasia         := '';
  FPorte            := '';
  FCNAE1            := '';
  FEndereco         := '';
  FNumero           := '';
  FComplemento      := '';
  FCEP              := '';
  FBairro           := '';
  FCidade           := '';
  FUF               := '';
  FSituacao         := '';
  FSituacaoEspecial := '';
  FCNPJ             := '';
  FDataSituacao     := 0;
  FDataSituacaoEspecial     := 0;
  FEndEletronico    := '';
  FTelefone         := '';
  FEFR              := '';
  FMotivoSituacaoCad:= '';
  FCodigoIBGE       := '';
  FInscricaoEstadual:= '';

  FCNAE2.Clear;
end;

function TACBrConsultaCNPJ.GetIBGE_UF: String;
begin
  Result := copy(fCodigoIBGE,1,2) ;
end;

function TACBrConsultaCNPJ.GetIniServicos: String;
begin
  if FIniServicos = '' then
    FIniServicos := ApplicationPath + FResourceName +'.ini';
  Result := FIniServicos;
end;

end.
