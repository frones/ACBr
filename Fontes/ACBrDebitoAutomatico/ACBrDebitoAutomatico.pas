{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit ACBrDebitoAutomatico;

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
  ACBrDebitoAutomaticoClass,
  ACBrDebitoAutomaticoConversao,
  ACBrDebitoAutomaticoArquivo,
  ACBrDebitoAutomaticoArquivoClass,
  ACBrDebitoAutomaticoConfiguracoes,
  ACBrDebitoAutomaticoInterface;

resourcestring
  ERR_SEM_BANCO = 'Nenhum Banco selecionado';

type
  EACBrDebitoAutomaticoException = class(Exception);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrDebitoAutomatico = class(TComponent)
  private
    FArquivo  : TACBrDebitoAutomaticoArquivoClass;
    FArquivos : TArquivos;
    FConfiguracoes: TConfiguracoes;
    FProvider: IACBrDebitoAutomaticoProvider;

    procedure SetArquivo(const Value: TACBrDebitoAutomaticoArquivoClass);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBanco(aBanco: TBanco = debNenhum);
    procedure SetProvider;

    procedure Limpar;
    function GravarTxt(const ANomeArquivo: String = ''): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método GravarTxtRemessa.' {$ENDIF};
    function GravarTxtRemessa(const ANomeArquivo: String = ''): Boolean;
    function LerTxt(const AArquivoTXT: String; ACarregarArquivo: Boolean = True): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método LerTxtRetorno.' {$ENDIF};
    function LerTxtRetorno(const AArquivoTXT: String): Boolean;

    property Arquivo: TACBrDebitoAutomaticoArquivoClass read FArquivo write SetArquivo;
    property Arquivos: TArquivos read FArquivos write FArquivos;
    property Provider: IACBrDebitoAutomaticoProvider read FProvider;
  published
    property Configuracoes: TConfiguracoes  read FConfiguracoes  write FConfiguracoes;
  end;

implementation

uses
  ACBrDebitoAutomaticoProviderManager;

{ TACBrDebitoAutomatico }

constructor TACBrDebitoAutomatico.Create(AOwner: TComponent);
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

destructor TACBrDebitoAutomatico.Destroy;
begin
  FConfiguracoes.Free;
  FArquivos.Free;

  if Assigned(FProvider) then FProvider := nil;

  inherited;
end;

function TACBrDebitoAutomatico.GravarTxt(const ANomeArquivo: String = ''): Boolean;
begin
  Result := GravarTxtRemessa(ANomeArquivo);
end;

function TACBrDebitoAutomatico.GravarTxtRemessa(const ANomeArquivo: String): Boolean;
begin
  if Self.Arquivos.Count <=0 then
    raise EACBrDebitoAutomaticoException.Create('ERRO: Nenhum arquivo adicionado.');

  if not Assigned(FProvider) then
    raise EACBrDebitoAutomaticoException.Create(ERR_SEM_BANCO);

  FProvider.Gerar(ANomeArquivo);

  Result := True;
end;

function TACBrDebitoAutomatico.LerTxt(const AArquivoTXT: String;
  ACarregarArquivo: Boolean): Boolean;
begin
  Result := LerTxtRetorno(AArquivoTXT)
end;

function TACBrDebitoAutomatico.LerTxtRetorno(const AArquivoTXT: String): Boolean;
begin
  FArquivos.Clear;
  FArquivos.LoadFromFile(AArquivoTXT);

  Result := True;
end;

procedure TACBrDebitoAutomatico.Limpar;
begin
  FArquivos.Clear;
  FArquivos.Configuracoes.Limpar;
  FConfiguracoes.Limpar;
end;

procedure TACBrDebitoAutomatico.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FArquivo <> nil) and
     (AComponent is TACBrDebitoAutomaticoArquivoClass) then
    FArquivo := nil;
end;

procedure TACBrDebitoAutomatico.SetArquivo(const Value: TACBrDebitoAutomaticoArquivoClass);
var
 OldValue: TACBrDebitoAutomaticoArquivoClass;
begin
  if Value <> FArquivo then
  begin
    if Assigned(FArquivo) then
      FArquivo.RemoveFreeNotification(Self);

    OldValue := FArquivo; // Usa outra variavel para evitar Loop Infinito
    FArquivo := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrDebitoAutomatico) then
        OldValue.ACBrDebitoAutomatico := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      Value.ACBrDebitoAutomatico := self;
    end;
  end;
end;

procedure TACBrDebitoAutomatico.SetBanco(aBanco: TBanco);
begin
  Configuracoes.Geral.Banco := aBanco;

  if aBanco <> debNenhum then
    SetProvider;
end;

procedure TACBrDebitoAutomatico.SetProvider;
begin
  if Assigned(FProvider) then
    FProvider := nil;

  FProvider := TACBrDebitoAutomaticoProviderManager.GetProvider(Self);

  if not Assigned(FProvider) then Exit;
end;

end.

