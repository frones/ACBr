{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrNFSeXDANFSeClass;

interface

uses
  SysUtils, Classes,
  ACBrBase, ACBrDFeReport,
  ACBrNFSeXClass, ACBrNFSeXConversao;

type
  { TPrestadorConfig }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TPrestadorConfig = class(TComponent)
  private
    FRazaoSocial: String;
    FNomeFantasia: String;
    FInscMunicipal: String;
    FInscEstadual: String;
    FCNPJ: String;
    FEndereco: String;
    FComplemento: String;
    FMunicipio: String;
    FUF: String;
    FEMail: String;
    FFone: String;
    FLogo: String;
    FNumero: String;
    FCEP: String;
    FBairro: String;
    FCodigoMunicipio: String;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
    property NomeFantasia: String read FNomeFantasia write FNomeFantasia;
    property InscricaoMunicipal: String read FInscMunicipal write FInscMunicipal;
    property InscricaoEstadual: String read FInscEstadual write FInscEstadual;
    property CNPJ: String read FCNPJ write FCNPJ;
    property Endereco: String read FEndereco write FEndereco;
    property Complemento: String read FComplemento write FComplemento;
    property Municipio: String read FMunicipio write FMunicipio;
    property UF: String read FUF write FUF;
    property Fone: String read FFone write FFone;
    property EMail: String read FEMail write FEMail;
    property Logo: String read FLogo write FLogo;
    property Numero: String read FNumero write FNumero;
    property CEP: String read FCEP write FCEP;
    property Bairro: String read FBairro write FBairro;
    property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
  end;

  { TTomadorConfig }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TTomadorConfig = class(TComponent)
  private
    FInscEstadual: String;
    FInscMunicipal: String;
    FFone: String;
    FEndereco: String;
    FComplemento: String;
    FEmail: String;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property InscricaoEstadual: String read FInscEstadual write FInscEstadual;
    property InscricaoMunicipal: String read FInscMunicipal write FInscMunicipal;
    property Fone: String read FFone write FFone;
    property Endereco: String read FEndereco write FEndereco;
    property Complemento: String read FComplemento write FComplemento;
    property Email: String read FEmail write FEmail;
  end;

  { TACBrNFSeXDANFSeClass }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFSeXDANFSeClass = class(TACBrDFeReport)
  private
    FProducao: TnfseSimNao;

    procedure SetACBrNFSe(const Value: TComponent);
    procedure ErroAbstract( const NomeProcedure: String );
  protected
    FACBrNFSe: TComponent;
    FPrefeitura: String;
    FOutrasInformacaoesImp : String;
    FFormatarNumeroDocumentoNFSe  : Boolean;
    FAtividade : String;
    FNFSeCancelada: boolean;
    FImprimeCanhoto: Boolean;
    FTipoDANFSE: TTipoDANFSE;
    FProvedor: TNFSeProvedor;
    FTamanhoFonte: Integer;
    FPrestador: TPrestadorConfig;
    FTomador: TTomadorConfig;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetSeparadorPathPDF(const aInitialPath: String): String; override;
    procedure SetDadosPrestador;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure VisualizarDANFSe(NFSe: TNFSe = nil); virtual;
    procedure ImprimirDANFSe(NFSe: TNFSe = nil); virtual;
    procedure ImprimirDANFSePDF(NFSe: TNFSe = nil); overload; virtual;
    procedure ImprimirDANFSePDF(AStream: TStream; NFSe: TNFSe = nil); overload; virtual;

  published
    property ACBrNFSe: TComponent  read FACBrNFSe write SetACBrNFSe;
    property Prestador: TPrestadorConfig read FPrestador;
    property Tomador: TTomadorConfig read FTomador;
    property OutrasInformacaoesImp: String read FOutrasInformacaoesImp write FOutrasInformacaoesImp;
    property Prefeitura: String read FPrefeitura write FPrefeitura;
    property Atividade: String read FAtividade write FAtividade;
    property Cancelada: Boolean read FNFSeCancelada write FNFSeCancelada;
    property ImprimeCanhoto: Boolean read FImprimeCanhoto write FImprimeCanhoto default False;
    property TipoDANFSE: TTipoDANFSE read FTipoDANFSE write FTipoDANFSE default tpPadrao;
    property Provedor: TNFSeProvedor read FProvedor write FProvedor;
    property TamanhoFonte: Integer read FTamanhoFonte write FTamanhoFonte;
    property FormatarNumeroDocumentoNFSe: Boolean read FFormatarNumeroDocumentoNFSe write FFormatarNumeroDocumentoNFSe;
    property Producao: TnfseSimNao read FProducao write FProducao;
  end;

implementation

uses
  StrUtils,
  ACBrUtil.Strings,
  ACBrNFSeX;

{ TPrestadorConfig }

constructor TPrestadorConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  RazaoSocial := '';
  NomeFantasia := '';
  InscricaoMunicipal := '';
  InscricaoEstadual := '';
  CNPJ := '';
  Endereco := '';
  Complemento := '';
  Municipio := '';
  UF := '';
  Fone := '';
  EMail := '';
  Logo := '';
  Numero := '';
  CEP := '';
  Bairro := '';
  CodigoMunicipio := '';
end;

{ TTomadorConfig }

constructor TTomadorConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Fone := '';
  Endereco := '';
  Complemento := '';
  Email := '';
  InscricaoEstadual := '';
  InscricaoMunicipal := '';
end;

{ TACBrNFSeXDANFSeClass }

constructor TACBrNFSeXDANFSeClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FACBrNFSe := nil;

  FPrestador := TPrestadorConfig.Create(Self);
  FPrestador.Name := 'Prestador';

  FTomador := TTomadorConfig.Create(Self);
  FTomador.Name := 'Tomador';

  {$IFDEF COMPILER6_UP}
  FPrestador.SetSubComponent(True);{ para gravar no DFM/XFM }
  FTomador.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}

  FPrefeitura := '';
  FTamanhoFonte := 6;
  FOutrasInformacaoesImp := '';
  FAtividade := '';
  FFormatarNumeroDocumentoNFSe := True;
  FNFSeCancelada := False;
  FProvedor := proNenhum;
end;

destructor TACBrNFSeXDANFSeClass.Destroy;
begin
  FPrestador.Free;
  FTomador.Free;

  inherited Destroy;
end;

procedure TACBrNFSeXDANFSeClass.ErroAbstract(const NomeProcedure: String);
begin
  raise Exception.Create( NomeProcedure );
end;

procedure TACBrNFSeXDANFSeClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrNFSe <> nil) and (AComponent is TACBrNFSeX) then
    FACBrNFSe := nil;
end;

procedure TACBrNFSeXDANFSeClass.SetACBrNFSe(const Value: TComponent);
var
  OldValue: TACBrNFSeX;
begin
  if Value <> FACBrNFSe then
  begin
    if Value <> nil then
      if not (Value is TACBrNFSeX) then
        raise Exception.Create('ACBrDANFSeX.ACBrNFSe deve ser do tipo TACBrNFSeX');

    if Assigned(FACBrNFSe) then
      FACBrNFSe.RemoveFreeNotification(Self);

    OldValue  := TACBrNFSeX(FACBrNFSe);  // Usa outra variavel para evitar Loop Infinito
    FACBrNFSe := Value;                 // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.DANFSe) then
        OldValue.DANFSe := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      TACBrNFSeX(Value).DANFSe := self;
    end;
  end;
end;

procedure TACBrNFSeXDANFSeClass.SetDadosPrestador;
begin
  // Usar a configuração do ACBrNFSeX se no XML não conter os dados do prestador

  with TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[0].NFSe do
  begin
    FPrestador.RazaoSocial := IfThen(Prestador.RazaoSocial <> '',
                              Prestador.RazaoSocial,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.RazSocial);

    FPrestador.CNPJ := IfThen(Prestador.IdentificacaoPrestador.CpfCnpj <> '',
                              Prestador.IdentificacaoPrestador.CpfCnpj,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.CNPJ);

    FPrestador.InscricaoMunicipal := IfThen(Prestador.IdentificacaoPrestador.InscricaoMunicipal <> '',
                              Prestador.IdentificacaoPrestador.InscricaoMunicipal,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.InscMun);

    FPrestador.InscricaoEstadual := IfThen(Prestador.IdentificacaoPrestador.InscricaoEstadual <> '',
                              Prestador.IdentificacaoPrestador.InscricaoEstadual,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.InscricaoEstadual);

    FPrestador.NomeFantasia := IfThen(Prestador.NomeFantasia <> '',
                              Prestador.NomeFantasia,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.NomeFantasia);

    FPrestador.Endereco := IfThen(Prestador.Endereco.Endereco <> '',
                              Prestador.Endereco.Endereco,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.Endereco);

    FPrestador.Numero := IfThen(Prestador.Endereco.Numero <> '',
                              Prestador.Endereco.Numero,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.Numero);

    FPrestador.Complemento := IfThen(Prestador.Endereco.Complemento <> '',
                              Prestador.Endereco.Complemento,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.Complemento);

    FPrestador.Bairro := IfThen(Prestador.Endereco.Bairro <> '',
                              Prestador.Endereco.Bairro,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.Bairro);

    FPrestador.CEP := IfThen(Prestador.Endereco.CEP <> '',
                              Prestador.Endereco.CEP,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.CEP);

    FPrestador.UF := IfThen(Prestador.Endereco.UF <> '',
                              Prestador.Endereco.UF,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.UF);

    FPrestador.Municipio := IfThen(Prestador.Endereco.xMunicipio <> '',
                              Prestador.Endereco.xMunicipio,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.Municipio);

    FPrestador.CodigoMunicipio := IfThen(Prestador.Endereco.CodigoMunicipio <> '',
                              Prestador.Endereco.CodigoMunicipio,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.CodigoMunicipio);

    FPrestador.Fone := IfThen(Prestador.Contato.Telefone <> '',
                              Prestador.Contato.Telefone,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.Telefone);

    FPrestador.EMail := IfThen(Prestador.Contato.EMail <> '',
                              Prestador.Contato.EMail,
                              TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Emitente.DadosEmitente.EMail);
  end;
end;

function TACBrNFSeXDANFSeClass.GetSeparadorPathPDF(const aInitialPath: String): String;
var
  dhEmissao: TDateTime;
  DescricaoModelo: String;
  ANFSe: TNFSe;
begin
  Result := aInitialPath;

  if Assigned(ACBrNFSe) then  // Se tem o componente ACBrNFSe
  begin
    if TACBrNFSeX(ACBrNFSe).NotasFiscais.Count > 0 then  // Se tem alguma Nota carregada
    begin
      ANFSe := TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[0].NFSe;
      if TACBrNFSeX(ACBrNFSe).Configuracoes.Arquivos.EmissaoPathNFSe then
        dhEmissao := ANFSe.DataEmissao
      else
        dhEmissao := Now;

      DescricaoModelo := '';
      if TACBrNFSeX(ACBrNFSe).Configuracoes.Arquivos.AdicionarLiteral then
      begin
        DescricaoModelo := TACBrNFSeX(ACBrNFSe).GetNomeModeloDFe;
      end;

      Result := TACBrNFSeX(ACBrNFSe).Configuracoes.Arquivos.GetPath(
                Result, DescricaoModelo,
                OnlyNumber(ANFSe.Prestador.IdentificacaoPrestador.CpfCnpj),
                OnlyNumber(ANFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual),
                dhEmissao);
    end;
  end;
end;

procedure TACBrNFSeXDANFSeClass.VisualizarDANFSe(NFSe: TNFSe);
begin
  ErroAbstract('VisualizarDANFSe');
end;

procedure TACBrNFSeXDANFSeClass.ImprimirDANFSe(NFSe: TNFSe);
begin
  ErroAbstract('ImprimirDANFSe');
end;

procedure TACBrNFSeXDANFSeClass.ImprimirDANFSePDF(NFSe: TNFSe);
begin
  ErroAbstract('ImprimirDANFSePDF');
end;

procedure TACBrNFSeXDANFSeClass.ImprimirDANFSePDF(AStream: TStream; NFSe: TNFSe);
begin
  ErroAbstract('ImprimirDANFSePDF');
end;

end.
