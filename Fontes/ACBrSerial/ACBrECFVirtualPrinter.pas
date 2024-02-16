{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrECFVirtualPrinter ;

interface
uses
  Classes, SysUtils,
  {$IFDEF NEXTGEN}
   ACBrBase,
  {$ENDIF}
  ACBrDevice, ACBrECFVirtualBuffer, ACBrECFClass,
  ACBrUtil.Strings,
  ACBrPosPrinter ;

const
  ACBrECFVirtualPrinter_VERSAO = '0.1.0a';

type

{ TACBrECFVirtualPrinter }

TACBrECFVirtualPrinter = class( TACBrECFVirtualBuffer )
  private
    FPosPrinter: TACBrPosPrinter;
    procedure SetPosPrinter(AValue: TACBrPosPrinter);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateVirtualClass ; override ;
    procedure AtivarPosPrinter;
  published
    property PosPrinter : TACBrPosPrinter read FPosPrinter write SetPosPrinter;

    property Colunas ;
    property NomeArqINI ;
    property NumSerie ;
    property NumECF ;
    property NumCRO ;
    property CNPJ ;
    property IE ;
    property IM ;

    property Cabecalho ;
    property CabecalhoItem ;
    property MascaraItem ;
  end ;

{ TACBrECFVirtualPrinterClass }

TACBrECFVirtualPrinterClass = class( TACBrECFVirtualBufferClass )
  private
    fsECFVirtualPrinter: TACBrECFVirtualPrinter;

  protected
    function GetDevice: TACBrDevice; override;
    function GetColunas: Integer; override;

    procedure AtivarVirtual ; override;
    procedure AbreDocumento ; override;

    function ColunasExpandido(): Integer; override;
  protected
    procedure Imprimir(const AString : AnsiString ) ; override ;

    function GetSubModeloECF: String ; override ;
    function GetNumVersao: String; override ;
  public
    Constructor Create( AECFVirtualPrinter : TACBrECFVirtualPrinter ); overload; virtual;
    Destructor Destroy  ; override ;
  end ;

implementation

Uses
  ACBrECF;

{ TACBrECFVirtualPrinter }

procedure TACBrECFVirtualPrinter.CreateVirtualClass;
begin
  fpECFVirtualClass := TACBrECFVirtualPrinterClass.create( self );
end;

procedure TACBrECFVirtualPrinter.AtivarPosPrinter;
begin
  if not Assigned( FPosPrinter ) then
    raise Exception.Create('Componente PosPrinter não associado');

  if FPosPrinter.Porta = '' then
  begin
    FPosPrinter.Porta := TACBrECF( ECF ).Porta;
    FPosPrinter.Device.ParamsString := TACBrECF( ECF ).Device.DeviceToString(false); 
  end;

  FPosPrinter.Ativar;
end;

procedure TACBrECFVirtualPrinter.SetPosPrinter(AValue: TACBrPosPrinter);
begin
  if AValue <> FPosPrinter then
  begin
     if Assigned(FPosPrinter) then
        FPosPrinter.RemoveFreeNotification(Self);

     FPosPrinter := AValue;

     if AValue <> nil then
        AValue.FreeNotification(self);
  end ;
end;

procedure TACBrECFVirtualPrinter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TACBrPosPrinter) and (FPosPrinter <> nil) then
       FPosPrinter := nil ;
  end;
end;

{ TACBrECFVirtualPrinterClass }

constructor TACBrECFVirtualPrinterClass.Create(
  AECFVirtualPrinter: TACBrECFVirtualPrinter);
begin
  fsECFVirtualPrinter := AECFVirtualPrinter;

  inherited create( AECFVirtualPrinter ) ;
end;

destructor TACBrECFVirtualPrinterClass.Destroy;
begin
  Desativar ;

  inherited Destroy ;
end;

procedure TACBrECFVirtualPrinterClass.AtivarVirtual;
begin
  fsECFVirtualPrinter.AtivarPosPrinter;

  inherited AtivarVirtual;
end;

function TACBrECFVirtualPrinterClass.GetSubModeloECF: String;
begin
  Result := 'VirtualPrinter' ;
end;

function TACBrECFVirtualPrinterClass.GetNumVersao: String ;
begin
  Result := ACBrECFVirtualPrinter_VERSAO ;
end;

function TACBrECFVirtualPrinterClass.GetDevice: TACBrDevice;
begin
  if Assigned(fsECFVirtualPrinter.PosPrinter) then
    Result := fsECFVirtualPrinter.PosPrinter.Device
  else
    Result := inherited GetDevice;
end;

function TACBrECFVirtualPrinterClass.GetColunas: Integer;
begin
  if Assigned(fsECFVirtualPrinter.PosPrinter) then
    Result := fsECFVirtualPrinter.PosPrinter.ColunasFonteNormal
  else
    Result := inherited GetColunas;
end;

function TACBrECFVirtualPrinterClass.ColunasExpandido: Integer;
begin
  Result := fsECFVirtualPrinter.PosPrinter.ColunasFonteExpandida;
end;

procedure TACBrECFVirtualPrinterClass.AbreDocumento ;
begin
  if not (fsECFVirtualPrinter.PosPrinter.ControlePorta or Device.EmLinha()) then
    raise EACBrECFERRO.Create(ACBrStr('Impressora: '+fpModeloStr+' não está pronta.')) ;

  inherited ;
end;

procedure TACBrECFVirtualPrinterClass.Imprimir(const AString: AnsiString);
Var
  OldAguardandoResposta : Boolean ;
begin
  OldAguardandoResposta := AguardandoResposta ;
  AguardandoResposta    := True ;
  try
    fsECFVirtualPrinter.PosPrinter.Imprimir(AString);

    if Assigned(QuantoImprimirLinha) then
      QuantoImprimirLinha(AString);

    if not fsECFVirtualPrinter.PosPrinter.ControlePorta then
    begin
      repeat
        Sleep(IntervaloAposComando);
      until Device.EmLinha() ;
    end;
  finally
    AguardandoResposta := OldAguardandoResposta ;
  end ;
end;

end.

