{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Alexandre Rocha Lima e Marcondes              }
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

unit ACBrCalculadora;

interface

uses
 ACBrBase,
 Classes, SysUtils,
 {$IFDEF VisualCLX}
   QControls, QGraphics, QForms
 {$ELSE}
   Controls,  Graphics,  Forms
 {$ENDIF};

type
  TACBrCalculadoraDisplayChange = procedure(Sender: TObject; Valor : Double) of object;

  { TACBrCalculadora }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrCalculadora = class ( TACBrComponent )
  private
    FBorderStyle : TFormBorderStyle;
    FTitulo: String ;
    FValor : Double ;
    FValorInicio: Double;
    FTexto : String ;
    FPrecisao: Integer;
    FSaiComEsc: Boolean;
    FCalcLeft: Integer;
    FCalcTop: Integer;
    FCor: TColor;
    FCorForm: TColor;
    FCentraliza: Boolean;
    FOnCalcKey: TKeyPressEvent;
    FOnDisplayChange: TACBrCalculadoraDisplayChange;
    procedure SetValor(AValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property Texto  : String read FTexto ;
  published
  { TODO : Adicionar evento OnMudaValor }

     property Valor  : Double read FValor  write SetValor stored false ;
     property Titulo : String read FTitulo write FTitulo ;
     property Precisao : Integer read FPrecisao write FPrecisao default 4 ;
     property SaiComEsc : Boolean read FSaiComEsc write FSaiComEsc
                       default true ;
     property Centraliza : Boolean read FCentraliza write FCentraliza
                       default false ;
     property CalcTop  : Integer read FCalcTop write FCalcTop default 0;
     property CalcLeft : Integer read FCalcLeft write FCalcLeft default 0;
     property CorDisplay : TColor read FCor write FCor default clLime ;
     property CorForm : TColor read FCorForm write FCorForm
        default clBtnFace ;
     property OnCalcKey : TKeyPressEvent read FOnCalcKey write FOnCalcKey;
     property OnDisplayChange: TACBrCalculadoraDisplayChange
                      read FOnDisplayChange write FOnDisplayChange;
     property BorderStyle : TFormBorderStyle read FBorderStyle write FBorderStyle ;
  end;

implementation
Uses {$IFDEF VisualCLX}
       QCalculadora
     {$ELSE}
       {$IFDEF FPC}
         LCalculadora
       {$ELSE}
         Calculadora
       {$ENDIF}
     {$ENDIF};

{ TACBrCalculadora }

procedure TACBrCalculadora.SetValor(AValue: Double);
begin
  if FValor = AValue then Exit;
  FValor := AValue;
  FValorInicio := AValue;
end;

constructor TACBrCalculadora.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  FTitulo     := 'ACBr Calculadora' ;
  FPrecisao   := 4 ;
  FSaiComEsc  := true ;
  FCor        := clLime ;
  FCentraliza := false ;
  FCorForm    := clBtnFace ;
  {$IFDEF VisualCLX}
   FBorderStyle:= fbsDialog;
  {$ELSE}
   FBorderStyle:= bsDialog;
  {$ENDIF}
end;

function TACBrCalculadora.Execute: Boolean;
var
  FrCalculadora : TFrCalculadora ;
begin
  //Result := False;
  FrCalculadora := TFrCalculadora.Create( Application ) ;
  try
     if FCentraliza then
        FrCalculadora.Position := poMainFormCenter
     else
        if (FCalcTop = 0) and (FCalcLeft = 0) then
           FrCalculadora.Position := poDefault
        else
         begin
           FrCalculadora.Top  := FCalcTop  ;
           FrCalculadora.Left := FCalcLeft ;
         end ;

     FrCalculadora.BorderStyle := FBorderStyle ;
     FrCalculadora.Caption := FTitulo ;
     FrCalculadora.Color := FCorForm;
     FrCalculadora.pValor.Font.Color := FCor ;
     FrCalculadora.ValorDisplay := FloatToStr( FValorInicio ) ;
     FrCalculadora.pSaiComEsc := FSaiComEsc ;
     FrCalculadora.pPrecisao := FPrecisao ;
     FrCalculadora.pOnCalKey := FOnCalcKey ;
     FrCalculadora.pOnDisplayChange := FOnDisplayChange ;
     FValorInicio := 0;

     Result := ( FrCalculadora.ShowModal = mrOk ) ;

     FTexto := FrCalculadora.ValorDisplay ;
     FValor := StrToFloat( FTexto ) ;
  finally
     FCalcTop  := FrCalculadora.Top  ;
     FCalcLeft := FrCalculadora.Left ;
     FrCalculadora.Free;
  end;
end;

end.
