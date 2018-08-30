{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{ Biblioteca multiplataforma de componentes Delphi para                        }
{ Emissão de Nota Fiscal de Serviço                                            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015 Italo Jurisato Junior                  }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeDANFSeClass;

interface

uses
  SysUtils, Classes, ACBrBase,
  pnfsNFSe, pnfsConversao;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
 TACBrNFSeDANFSeClass = class( TACBrComponent )
  private
    function GetPathPDF: String;
    procedure SetPathPDF(const Value: String);
    procedure SetNFSe(const Value: TComponent);
    procedure ErroAbstract(NomeProcedure: String);
  protected
    FACBrNFSe: TComponent;
    FLogo: String;
    FSistema: String;
    FUsuario: String;
    FPathPDF: String;
    FImpressora: String;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
    FNumCopias: Integer;
    FExpandirLogoMarca: Boolean;
    FFax: String;
    FSite: String;
    FEmail: String;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FPrestLogo: String;
    FPrefeitura: String;
    FRazaoSocial: String;
    FEndereco: String;
    FComplemento: String;
    FFone: String;
    FMunicipio: String;
    FOutrasInformacaoesImp: String;
    FInscMunicipal: String;
    FT_InscEstadual: String;
    FT_InscMunicipal: String;
    FT_Fone: String;
    FT_Endereco: String;
    FT_Complemento: String;
    FT_Email: String;
    FEMail_Prestador: String;
		FFormatarNumeroDocumentoNFSe: Boolean;
    FUF: String;
    FAtividade: String;
    FNFSeCancelada: boolean;
    FImprimeCanhoto: Boolean;
    FTipoDANFSE: TTipoDANFSE;
    FProvedor: TNFSeProvedor;
    FTamanhoFonte: Integer;
    FUsarSeparadorPathPDF: Boolean;
    FDetalharServico: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure VisualizarDANFSe(NFSe: TNFSe = nil); virtual;
    procedure ImprimirDANFSe(NFSe: TNFSe = nil); virtual;
    procedure ImprimirDANFSePDF(NFSe: TNFSe = nil); virtual;
  published
    property ACBrNFSe: TComponent read FACBrNFSe write SetNFSe;
    property Logo: String read FLogo write FLogo;
    property Sistema: String read FSistema write FSistema;
    property Usuario: String read FUsuario write FUsuario;
    property PathPDF: String read GetPathPDF write SetPathPDF;
    property UsarSeparadorPathPDF: Boolean read FUsarSeparadorPathPDF write FUsarSeparadorPathPDF default False;
    property Impressora: String read FImpressora write FImpressora;
    property MostrarPreview: Boolean read FMostrarPreview write FMostrarPreview;
    property MostrarStatus: Boolean read FMostrarStatus write FMostrarStatus;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property ExpandirLogoMarca: Boolean read FExpandirLogoMarca write FExpandirLogoMarca default false;
    property Fax : String read FFax write FFax;
    property Site: String read FSite write FSite;
    property Email: String read FEmail write FEmail;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property PrestLogo: String read FPrestLogo write FPrestLogo;
    property Prefeitura: String read FPrefeitura write FPrefeitura;

    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
    property UF: String read FUF write FUF;
    property Endereco: String read FEndereco write FEndereco;
    property Complemento: String read FComplemento write FComplemento;
    property Fone: String read FFone write FFone;
    property Municipio: String read FMunicipio write FMunicipio;
    property OutrasInformacaoesImp: String read FOutrasInformacaoesImp write FOutrasInformacaoesImp;
    property InscMunicipal: String read FInscMunicipal write FInscMunicipal;
    property EMail_Prestador: String read FEMail_Prestador write FEMail_Prestador;

    property T_InscEstadual: String read FT_InscEstadual write FT_InscEstadual;
    property T_InscMunicipal: String read FT_InscMunicipal write FT_InscMunicipal;
    property T_Fone: String read FT_Fone write FT_Fone;

    property T_Endereco: String read FT_Endereco write FT_Endereco;
    property T_Complemento: String read FT_Complemento write FT_Complemento;
    property T_Email: String read FT_Email write FT_Email;

    property Atividade: String read FAtividade write FAtividade;
    property NFSeCancelada: Boolean read FNFSeCancelada write FNFSeCancelada;
    property ImprimeCanhoto: Boolean read FImprimeCanhoto write FImprimeCanhoto default False;

    property TipoDANFSE: TTipoDANFSE read FTipoDANFSE write FTipoDANFSE default tpPadrao;
    property Provedor: TNFSeProvedor read FProvedor write FProvedor;
    property TamanhoFonte: Integer read FTamanhoFonte write FTamanhoFonte;
    property FormatarNumeroDocumentoNFSe: Boolean read FFormatarNumeroDocumentoNFSe write FFormatarNumeroDocumentoNFSe;
    property DetalharServico: Boolean read FDetalharServico write FDetalharServico default False;
  end;

implementation

uses
  ACBrNFSe, ACBrUtil;

{ TACBrNFSeDANFSeClass }

constructor TACBrNFSeDANFSeClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FACBrNFSe       := nil;
  FLogo           := '';
  FSistema        := '';
  FUsuario        := '';
  FPathPDF        := '';
  FImpressora     := '';
  FMostrarPreview := True;
  FMostrarStatus  := True;
  FNumCopias      := 1;
  FFax            := '';
  FSite           := '';
  FEmail          := '';
  FMargemInferior := 0.8;
  FMargemSuperior := 0.8;
  FMargemEsquerda := 0.6;
  FMargemDireita  := 0.51;
  FPrestLogo      := '';
  FPrefeitura     := '';
  FRazaoSocial    := '';
  FEndereco       := '';
  FComplemento    := '';
  FFone           := '';
  FMunicipio      := '';
  FTamanhoFonte   := 6;
  FDetalharServico:= False;

  FOutrasInformacaoesImp := '';
  FInscMunicipal         := '';
  FEMail_Prestador       := '';
  FUF                    := '';
  FT_InscEstadual        := '';
  FT_InscMunicipal       := '';
  FAtividade             := '';
  FT_Fone                := '';
  FT_Endereco            := '';
  FT_Complemento         := '';
  FT_Email               := '';
  FFormatarNumeroDocumentoNFSe := True;
  FNFSeCancelada := False;

  FProvedor := proNenhum;
end;

destructor TACBrNFSeDANFSeClass.Destroy;
begin

  inherited Destroy;
end;

procedure TACBrNFSeDANFSeClass.ErroAbstract(NomeProcedure: String);
begin
  raise Exception.Create( NomeProcedure );
end;

procedure TACBrNFSeDANFSeClass.VisualizarDANFSe(NFSe: TNFSe);
begin
  ErroAbstract('Visualizar');
end;

procedure TACBrNFSeDANFSeClass.ImprimirDANFSe(NFSe: TNFSe);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrNFSeDANFSeClass.ImprimirDANFSePDF(NFSe: TNFSe);
begin
  ErroAbstract('ImprimirPDF');
end;

procedure TACBrNFSeDANFSeClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrNFSe <> nil) and (AComponent is TACBrNFSe) then
    FACBrNFSe := nil;
end;

procedure TACBrNFSeDANFSeClass.SetNFSe(const Value: TComponent);
var
  OldValue: TACBrNFSe;
begin
  if Value <> FACBrNFSe then
  begin
    if Value <> nil then
      if not (Value is TACBrNFSe) then
        raise Exception.Create('ACBrDANFSe.NFSe deve ser do tipo TACBrNFSe');

    if Assigned(FACBrNFSe) then
      FACBrNFSe.RemoveFreeNotification(Self);

    OldValue  := TACBrNFSe(FACBrNFSe);  // Usa outra variavel para evitar Loop Infinito
    FACBrNFSe := Value;                 // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.DANFSe) then
        OldValue.DANFSe := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      TACBrNFSe(Value).DANFSe := self;
    end;
  end;
end;

function TACBrNFSeDANFSeClass.GetPathPDF: String;
var
  dhEmissao: TDateTime;
  DescricaoModelo: String;
  ANFSe: TNFSe;
begin
  if (csDesigning in ComponentState) then
  begin
    Result := FPathPDF;
    Exit;
  end;

  Result := Trim(FPathPDF);

  if EstaVazio(Result) then  // Se não pode definir o Parth, use o Path da Aplicaçao
    Result := PathWithDelim( ExtractFilePath(ParamStr(0))) + 'pdf';

  if FUsarSeparadorPathPDF then
  begin
    if Assigned(ACBrNFSe) then  // Se tem o componente ACBrNFSe
    begin
      if TACBrNFSe(ACBrNFSe).NotasFiscais.Count > 0 then  // Se tem alguma Nota carregada
      begin
        ANFSe := TACBrNFSe(ACBrNFSe).NotasFiscais.Items[0].NFSe;
        if TACBrNFSe(ACBrNFSe).Configuracoes.Arquivos.EmissaoPathNFSe then
          dhEmissao := ANFSe.DataEmissao
        else
          dhEmissao := Now;

        DescricaoModelo := '';
        if TACBrNFSe(ACBrNFSe).Configuracoes.Arquivos.AdicionarLiteral then
        begin
           DescricaoModelo := TACBrNFSe(ACBrNFSe).GetNomeModeloDFe;
        end;

        Result := TACBrNFSe(ACBrNFSe).Configuracoes.Arquivos.GetPath(
                         Result,
                         DescricaoModelo,
                         OnlyNumber(ANFSe.PrestadorServico.IdentificacaoPrestador.CNPJ),
                         dhEmissao);
      end;
    end;
  end;

  Result := PathWithDelim( Result );
end;

procedure TACBrNFSeDANFSeClass.SetPathPDF(const Value: String);
begin
  if Trim(Value) <> '' then
    FPathPDF := IncludeTrailingPathDelimiter(Value);
end;

end.
