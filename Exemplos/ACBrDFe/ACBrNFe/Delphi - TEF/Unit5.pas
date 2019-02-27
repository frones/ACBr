unit Unit5; 

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, 
   Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls,
  ACBrTEFDCliSiTef;

type

{$IFNDEF FPC}
 {$R *.dfm}
{$ELSE}
 {$R *.lfm}
{$ENDIF}

{ TForm5 }

  TForm5 = class(TForm)
     BitBtn1 : TBitBtn;
     BitBtn2 : TBitBtn;
     BitBtn3: TBitBtn;
     Edit1 : TEdit;
     Panel1 : TPanel;
     procedure Edit1KeyPress(Sender : TObject; var Key : char);
     procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
     procedure FormCreate(Sender : TObject);
     procedure FormShow(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
    TipoCampo : Integer;
    Operacao  : TACBrTEFDCliSiTefOperacaoCampo;
    TamanhoMinimo, TamanhoMaximo : Integer ;
  end; 

var
  Form5 : TForm5; 

implementation

{ TForm5 }

procedure TForm5.FormCreate(Sender : TObject);
begin
  TamanhoMinimo := 0 ;
  TamanhoMaximo := 0 ;
  Operacao      := tcString;
  TipoCampo     := 0 ;
end;

procedure TForm5.Edit1KeyPress(Sender : TObject; var Key : char);
begin
   if Key in [#13,#8] then exit ;  { Enter e BackSpace, OK }

   if Operacao in [tcDouble, tcCMC7] then
      if not (Key in ['0'..'9', ',']) then    { Apenas números }
         Key := #0 ;

   if (TamanhoMaximo > 0) and (Length( Edit1.Text ) >= TamanhoMaximo) then
      Key := #0 ;
end;

procedure TForm5.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
   if (ModalResult = mrOK) and (TamanhoMinimo > 0) then
   begin
      if Length( Edit1.Text ) < TamanhoMinimo then
      begin
         ShowMessage('O Tamanho Mínimo para este campo e: '+IntToStr(TamanhoMinimo) );
         CanClose := False ;
         Edit1.SetFocus;
      end;
   end;
end;

procedure TForm5.FormShow(Sender : TObject);
begin
   if Operacao = tcDouble then
      Edit1.Text := '0,00' ;
   Edit1.SetFocus;
end;

end.

