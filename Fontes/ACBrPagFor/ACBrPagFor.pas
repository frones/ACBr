{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrPagFor;

interface

uses
  Classes, 
  SysUtils, 
  Contnrs,
  {$IFNDEF NOGUI}
	  {$IFDEF CLX} 
		QDialogs,
	  {$ELSE} 
		Dialogs,
	  {$ENDIF}
  {$ENDIF}
  ACBrBase,
  ACBrPagForClass, 
  ACBrPagForConversao,
  ACBrPagForArquivo, 
  ACBrPagForArquivoClass, 
  ACBrPagForConfiguracoes,
  ACBrPagForInterface;

resourcestring
  ERR_SEM_BANCO = 'Nenhum Banco selecionado';

type
  EACBrPagForException = class(Exception);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPagFor = class(TACBrComponent)
  private
    FArquivo  : TACBrPagForArquivoClass;
    FArquivos : TArquivos;
    FConfiguracoes: TConfiguracoes;
    FProvider: IACBrPagForProvider;

    procedure SetArquivo(const Value: TACBrPagForArquivoClass);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBanco(aBanco: TBanco = PagNenhum);
    procedure SetProvider;

    procedure Limpar;
    function GravarTxt(const ANomeArquivo: String = ''): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método GravarTxtRemessa.' {$ENDIF};
    function GravarTxtRemessa(const ANomeArquivo: String = ''): Boolean;
    function LerTxt(const AArquivoTXT: String; ACarregarArquivo: Boolean = True): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método LerTxtRetorno.' {$ENDIF};
    function LerTxtRetorno(const AArquivoTXT: String): Boolean;

    property Arquivo: TACBrPagForArquivoClass read FArquivo write SetArquivo;
    property Arquivos: TArquivos read FArquivos write FArquivos;
    property Provider: IACBrPagForProvider read FProvider;
  published
    property Configuracoes: TConfiguracoes  read FConfiguracoes  write FConfiguracoes;
  end;

implementation

uses
  ACBrPagForProviderManager;

{ TACBrPagFor }

constructor TACBrPagFor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FConfiguracoes := TConfiguracoes.Create( self );
  FConfiguracoes.Name := 'Configuracoes';

  {$IFDEF COMPILER6_UP}
   FConfiguracoes.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}

  FArquivos := TArquivos.Create(Self);
  FArquivos.Configuracoes := FConfiguracoes;
end;

destructor TACBrPagFor.Destroy;
begin
  FConfiguracoes.Free;
  FArquivos.Free;

  if Assigned(FProvider) then FProvider := nil;

  inherited;
end;

function TACBrPagFor.GravarTxt(const ANomeArquivo: String = ''): Boolean;
begin
  Result := GravarTxtRemessa(ANomeArquivo);
end;

function TACBrPagFor.GravarTxtRemessa(const ANomeArquivo: String): Boolean;
begin
  if Self.Arquivos.Count <=0 then
    raise EACBrPagForException.Create('ERRO: Nenhum arquivo adicionado.');

  if not Assigned(FProvider) then
    raise EACBrPagForException.Create(ERR_SEM_BANCO);

  FProvider.Gerar(ANomeArquivo);

  Result := True;
end;

function TACBrPagFor.LerTxt(const AArquivoTXT: String;
  ACarregarArquivo: Boolean): Boolean;
begin
  Result := LerTxtRetorno(AArquivoTXT)
end;

function TACBrPagFor.LerTxtRetorno(const AArquivoTXT: String): Boolean;
begin
  FArquivos.Clear;
  FArquivos.LoadFromFile(AArquivoTXT);

  Result := True;
end;

procedure TACBrPagFor.Limpar;
begin
  FArquivos.Clear;
  FArquivos.Configuracoes.Limpar;
  FConfiguracoes.Limpar;
end;

procedure TACBrPagFor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FArquivo <> nil) and
     (AComponent is TACBrPagForArquivoClass) then
    FArquivo := nil;
end;

procedure TACBrPagFor.SetArquivo(const Value: TACBrPagForArquivoClass);
var
 OldValue: TACBrPagForArquivoClass;
begin
  if Value <> FArquivo then
  begin
    if Assigned(FArquivo) then
      FArquivo.RemoveFreeNotification(Self);

    OldValue := FArquivo; // Usa outra variavel para evitar Loop Infinito
    FArquivo := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrPagFor) then
        OldValue.ACBrPagFor := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      Value.ACBrPagFor := self;
    end;
  end;
end;

procedure TACBrPagFor.SetBanco(aBanco: TBanco);
begin
  Configuracoes.Geral.Banco := aBanco;

  if aBanco <> pagNenhum then
    SetProvider;
end;

procedure TACBrPagFor.SetProvider;
begin
  if Assigned(FProvider) then
    FProvider := nil;

  FProvider := TACBrPagForProviderManager.GetProvider(Self);

  if not Assigned(FProvider) then Exit;
end;

end.

