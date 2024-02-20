{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrBPeDABPEClass;

interface

uses
  SysUtils, Classes,
  ACBrBase,
  ACBrBPeClass,
  pcnConversao,
  ACBrDFeReport;

type

  { TACBrBPeDABPEClass }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBPeDABPEClass = class( TACBrDFeReport )
  private
    procedure SetBPe(const Value: TComponent);
    procedure ErroAbstract(const NomeProcedure: String);

  protected
   function GetSeparadorPathPDF(const aInitialPath: String): String; override;

  protected
    FACBrBPe: TComponent;
    FTipoDABPE: TpcnTipoImpressao;
    FProtocolo: String;
    FCancelada: Boolean;
    FViaConsumidor: Boolean;
    FImprimeNomeFantasia: Boolean;
    FImprimeLogoLateral: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDABPE(BPe: TBPe = nil); virtual;
    procedure ImprimirDABPECancelado(BPe: TBPe = nil); virtual;
    procedure ImprimirDABPEResumido(BPe: TBPe = nil); virtual;
    procedure ImprimirDABPEPDF(BPe: TBPe = nil); virtual;
    procedure ImprimirDABPEResumidoPDF(BPe: TBPe = nil); virtual;
    procedure ImprimirEVENTO(BPe: TBPe = nil); virtual;
    procedure ImprimirEVENTOPDF(BPe: TBPe = nil); virtual;

  published
    property ACBrBPe: TComponent                     read FACBrBPe                        write SetBPe;
    property TipoDABPE: TpcnTipoImpressao            read FTipoDABPE                      write FTipoDABPE;
    property Protocolo: String                       read FProtocolo                      write FProtocolo;
    property Cancelada: Boolean                      read FCancelada                      write FCancelada;
    property ViaConsumidor: Boolean                  read FViaConsumidor                  write FViaConsumidor;
    property ImprimeNomeFantasia: Boolean            read FImprimeNomeFantasia            write FImprimeNomeFantasia;
    property ImprimeLogoLateral: Boolean read FImprimeLogoLateral write FImprimeLogoLateral default False;
  end;

implementation

uses
  ACBrBPe;

//DABPE CLASS
constructor TACBrBPeDABPEClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FACBrBPe    := nil;

  FProtocolo    := '';
  FCancelada := False;
  FViaConsumidor := True;
  FImprimeNomeFantasia := False;
  FImprimeLogoLateral  := False;
end;

destructor TACBrBPeDABPEClass.Destroy;
begin

  inherited Destroy;
end;

procedure TACBrBPeDABPEClass.ImprimirDABPE(BPe : TBPe = nil);
begin
  ErroAbstract('ImprimirDABPE');
end;

procedure TACBrBPeDABPEClass.ImprimirDABPECancelado(BPe: TBPe = nil);
begin
  ErroAbstract('ImprimirDABPECancelado');
end;

procedure TACBrBPeDABPEClass.ImprimirDABPEResumido(BPe : TBPe = nil);
begin
  ErroAbstract('ImprimirDABPEResumido');
end;

procedure TACBrBPeDABPEClass.ImprimirDABPEPDF(BPe : TBPe = nil);
begin
  ErroAbstract('ImprimirDABPEPDF');
end;

procedure TACBrBPeDABPEClass.ImprimirDABPEResumidoPDF(BPe: TBPe = nil);
begin
  ErroAbstract('ImprimirDABPEResumidoPDF');
end;

procedure TACBrBPeDABPEClass.ImprimirEVENTO(BPe: TBPe = nil);
begin
  ErroAbstract('ImprimirEVENTO');
end;

procedure TACBrBPeDABPEClass.ImprimirEVENTOPDF(BPe: TBPe = nil);
begin
  ErroAbstract('ImprimirEVENTOPDF');
end;

procedure TACBrBPeDABPEClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrBPe <> nil) and (AComponent is TACBrBPe) then
    FACBrBPe := nil;
end;

procedure TACBrBPeDABPEClass.SetBPe(const Value: TComponent);
  Var OldValue : TACBrBPe;
begin
  if Value <> FACBrBPe then
  begin
    if Value <> nil then
      if not (Value is TACBrBPe) then
        raise EACBrBPeException.Create('ACBrDABPE.BPe deve ser do tipo TACBrBPe');

    if Assigned(FACBrBPe) then
      FACBrBPe.RemoveFreeNotification(Self);

    OldValue := TACBrBPe(FACBrBPe);   // Usa outra variavel para evitar Loop Infinito
    FACBrBPe := Value;                 // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.DABPE) then
        OldValue.DABPE := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      TACBrBPe(Value).DABPE := self;
    end;
  end;
end;

procedure TACBrBPeDABPEClass.ErroAbstract(const NomeProcedure: String);
begin
  raise EACBrBPeException.Create(NomeProcedure + ' não implementado em: ' + ClassName);
end;

function TACBrBPeDABPEClass.GetSeparadorPathPDF(const aInitialPath: String): String;
var
  dhEmissao: TDateTime;
  DescricaoModelo: String;
  ABPe: TBPe;
begin
  Result := aInitialPath;
  
  if Assigned(ACBrBPe) then  // Se tem o componente ACBrBPe
  begin
    if TACBrBPe(ACBrBPe).Bilhetes.Count > 0 then  // Se tem algum Bilhete carregado
    begin
      ABPe := TACBrBPe(ACBrBPe).Bilhetes.Items[0].BPe;
      if TACBrBPe(ACBrBPe).Configuracoes.Arquivos.EmissaoPathBPe then
        dhEmissao := ABPe.Ide.dhEmi
      else
        dhEmissao := Now;

      DescricaoModelo := 'BPe';
      Result := TACBrBPe(FACBrBPe).Configuracoes.Arquivos.GetPath(
                         Result,
                         DescricaoModelo,
                         ABPe.Emit.CNPJ,
                         ABPe.Emit.IE,
                         dhEmissao,
                         DescricaoModelo);
    end;
  end;
end;

end.
