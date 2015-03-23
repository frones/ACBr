{$I ACBr.inc}

unit CHQTeste1;

interface
uses ACBrCHQ, ACBrECF, ACBrBase, ACBrExtenso, StdCtrls, Controls,
  Forms, Dialogs, Buttons, Graphics, Classes, ExtCtrls ;

type
  TfrCHQ = class(TForm)
    ACBrExtenso1: TACBrExtenso;
    ACBrECF1: TACBrECF;
    ACBrCHQ1: TACBrCHQ;
    pFrente: TPanel;
    Image1: TImage;
    lExtenso2: TLabel;
    lExtenso1: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    edValor: TEdit;
    edFavorecido: TEdit;
    edDia: TEdit;
    edCidade: TEdit;
    edBanco: TEdit;
    edAno: TEdit;
    cbxPorta: TComboBox;
    cbxModelo: TComboBox;
    cbVerifica: TCheckBox;
    cbMes: TComboBox;
    bVerso: TButton;
    bImprimir: TButton;
    bCMC7: TButton;
    pVerso: TPanel;
    Image2: TImage;
    bImpVerso: TButton;
    bFrente: TButton;
    mVerso: TMemo;
    sbECF: TSpeedButton;
    procedure edValorExit(Sender: TObject);
    procedure edValorKeyPress(Sender: TObject; var Key: Char);
    procedure bImprimirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edBancoExit(Sender: TObject);
    procedure sbECFClick(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bCMC7Click(Sender: TObject);
    procedure bVersoClick(Sender: TObject);
    procedure bFrenteClick(Sender: TObject);
    procedure bImpVersoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frCHQ: TfrCHQ;

implementation

uses SysUtils,
   {$IFDEF Delphi6_UP} DateUtils, {$ELSE} ACBrD5,{$ENDIF}
   ACBrUtil, ACBrCHQPerto ;

{$R *.dfm}

procedure TfrCHQ.FormCreate(Sender: TObject);
begin
  cbxModelo.ItemIndex := 2 ;
  
  pFrente.Height := Image1.Height + 1 ;
  pFrente.Width  := Image1.Width  + 1 ;
  pVerso.Height  := Image1.Height + 1 ;
  pVerso.Height  := Image1.Height + 1 ;
  ClientHeight   := pFrente.Height  ;
  ClientWidth    := pFrente.Width  ;
  pFrente.Visible := true ;
  pVerso.Visible  := false ;

  edDia.Text := IntToStr( DayOf(now) ) ;
  cbMes.Text := cbMes.Items[ MonthOf(now)-1 ] ;
  edAno.Text := IntToStr( YearOf(now) ) ;

  cbxModeloChange( Sender );
  cbxPortaChange( Sender );
end;

procedure TfrCHQ.edValorExit(Sender: TObject);
Var Texto, Tracos : String ;
    Pos, Tamanho : Integer ;
begin
  ACBrExtenso1.Valor := StrToFloatDef( edValor.Text, 0 ) ;

  { Verificando se o extenso cabe na linha de cima }
  Tracos:= '' ;
  Texto := '( '+Trim(ACBrExtenso1.Texto)+' )' ;
  Pos   := Length( Texto );
  with frCHQ.Canvas do
  begin
     Font    := lExtenso1.Font ;
     Tamanho := TextWidth(Texto) ;

     while (Tamanho > lExtenso1.Width) do
     begin
        { Acha um espaço }
        while (Texto[Pos] <> ' ') and (Pos > 0) do
           Pos := Pos - 1 ;

        Pos := Pos - 1 ;
        Tamanho := TextWidth(copy(Texto,1,Pos)) ;
     end ;

     { Inserindo traços no inicio }
     if Pos < Length( Texto ) then
      begin
        while (Tamanho < lExtenso1.Width) do
        begin
           Tracos  := Tracos + '-' ;
           Tamanho := TextWidth(Tracos + copy(Texto,1,Pos)) ;
        end ;
        Tracos := copy(Tracos,1,Length(Tracos)-1) ;
      end ;
  end ;

  lExtenso1.Caption := Tracos + copy(Texto,1,Pos) ;
  lExtenso2.Caption := copy(Texto,Pos+2, Length(Texto) )
end;

procedure TfrCHQ.edValorKeyPress(Sender: TObject; var Key: Char);
begin
  if Key in [',','.'] then
     Key := DecimalSeparator ;
end;

procedure TfrCHQ.bImprimirClick(Sender: TObject);
Var Data : TDateTime ;
begin
  if cbMes.Items.IndexOf( cbMes.Text ) < 0 then
  begin
     MessageDlg('Mês inválido',mtError,[mbOk],0) ;
     cbMes.SetFocus ;
     exit ;
  end ;

  if StrToFloatDef( edValor.Text, 0 ) = 0 then
  begin
     MessageDlg('Valor inválido',mtError,[mbOk],0) ;
     edValor.SetFocus ;
     exit ;
  end ;

  Data := EncodeDate(StrToIntDef(edAno.Text,YearOf(now)),
                     cbMes.Items.IndexOf( cbMes.Text ) + 1,
                     StrToIntDef(edDia.Text,DayOf(now))  ) ;

  if cbVerifica.Checked and ( not ACBrCHQ1.ChequePronto ) then
  begin
     MessageDlg('Cheque não está posicionado na Impressora',mtError,[mbOk],0) ;
     bImprimir.SetFocus ;
     exit ;
  end ;

  ACBrCHQ1.Banco := edBanco.Text ;
  ACBrCHQ1.Valor := StrToFloat( edValor.Text ) ;
  ACBrCHQ1.Data  := Data ;
  ACBrCHQ1.Favorecido := edFavorecido.Text ;
  ACBrCHQ1.Cidade := edCidade.Text ;

  ACBrCHQ1.ImprimirCheque ;
end;

procedure TfrCHQ.edBancoExit(Sender: TObject);
begin
  { IntToStrZero -> ACBrUtil.pas }
  edBanco.Text := IntToStrZero( StrToIntDef( edBanco.Text, 0), 3) ;
end;

procedure TfrCHQ.sbECFClick(Sender: TObject);
begin
  ACBrECF1.Ativar ;
  ACBrECF1.TestarDialog ;
end;

procedure TfrCHQ.cbxModeloChange(Sender: TObject);
Var OldModelo : TACBrCHQModelo ;
begin
  ACBrCHQ1.Desativar ;
  OldModelo := ACBrCHQ1.Modelo ;

  try
     ACBrCHQ1.Modelo := TACBrCHQModelo( cbxModelo.ItemIndex ) ;

     if ACBrCHQ1.Modelo = chqImpressoraECF then
     begin
        ACBrCHQ1.ECF := ACBrECF1 ;
        ACBrECF1.Ativar ;
     end ;
  except
     ACBrCHQ1.Modelo := OldModelo ;
  end ;

  cbxModelo.ItemIndex := Integer( ACBrCHQ1.Modelo ) ;
  cbxPorta.Text  := ACBrCHQ1.Porta ;
  
  bCMC7.Visible := (ACBrCHQ1.Modelo = chqPerto) ;
{  sbECF.Visible := (ACBrCHQ1.Modelo = chqImpressoraECF) ;}
end;

procedure TfrCHQ.cbxPortaChange(Sender: TObject);
begin
  if ACBrCHQ1.Modelo <> chqImpressoraECF then
  begin
    ACBrCHQ1.Desativar ;
    ACBrCHQ1.Porta := cbxPorta.Text ;
  end ;
end;

procedure TfrCHQ.FormShow(Sender: TObject);
begin
  edBanco.SetFocus ;
  edBanco.SelectAll ;
end;

procedure TfrCHQ.bCMC7Click(Sender: TObject);
begin
  if ACBrCHQ1.Modelo <> chqPerto then
     raise Exception.Create('Apenas impressora PertoCheck lê CMC7')
  else
     with ACBrCHQ1.CHQ as TACBrCHQPerto do
     begin
        MessageDlg('Banco: '+BancoLido+#10+
                   ' Agencia: '+AgenciaLida+#10+
                   ' Conta: '+ContaLida+#10+      
                   ' Cheque: '+ChequeLido+#10+
                   ' Comp.: '+CompLida,mtInformation,[mbOk],0) ;
     end ;
end;

procedure TfrCHQ.bVersoClick(Sender: TObject);
begin
  pFrente.Visible := False ;
  pVerso.Visible  := True ;
end;

procedure TfrCHQ.bFrenteClick(Sender: TObject);
begin
  pVerso.Visible  := False ;
  pFrente.Visible := True ;
end;

procedure TfrCHQ.bImpVersoClick(Sender: TObject);
begin
  ACBrCHQ1.ImprimirVerso( mVerso.Lines );
end;

end.
