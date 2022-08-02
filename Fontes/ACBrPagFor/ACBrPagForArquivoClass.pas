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

unit ACBrPagForArquivoClass;

interface

uses
  Forms, SysUtils, Classes,
  ACBrPagForClass, ACBrPagForConversao;

type
  TACBrPagForArquivoClass = class( TComponent )
   private
     procedure SetACBrPagFor(const Value: TComponent);
   protected
     FACBrPagFor : TComponent;
     procedure ErroAbstract(const NomeProcedure : String );
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
   published
     property ACBrPagFor: TComponent read FACBrPagFor write SetACBrPagFor;
  end;

implementation

uses
  ACBrPagFor;

{ TACBrPagForArquivoClass }

constructor TACBrPagForArquivoClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FACBrPagFor := nil;
end;

destructor TACBrPagForArquivoClass.Destroy;
begin

  inherited Destroy;
end;

procedure TACBrPagForArquivoClass.ErroAbstract(const NomeProcedure: String);
begin
  raise EACBrPagForException.Create( NomeProcedure );
end;

procedure TACBrPagForArquivoClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrPagFor <> nil) and (AComponent is TACBrPagFor) then
    FACBrPagFor := nil;
end;

procedure TACBrPagForArquivoClass.SetACBrPagFor(const Value: TComponent);
var
  OldValue: TACBrPagFor;
begin
  if Value <> FACBrPagFor then
  begin
    if Value <> nil then
      if not (Value is TACBrPagFor) then
        raise EACBrPagForException.Create('ACBrPagFor deve ser do tipo TACBrPagFor');

    if Assigned(FACBrPagFor) then
      FACBrPagFor.RemoveFreeNotification(Self);

    OldValue := TACBrPagFor(FACBrPagFor);   // Usa outra variavel para evitar Loop Infinito
    FACBrPagFor := Value;                 // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.Arquivo) then
        OldValue.Arquivo := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      TACBrPagFor(Value).Arquivo := self;
    end;
  end;
end;

end.
