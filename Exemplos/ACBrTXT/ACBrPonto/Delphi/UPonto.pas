unit UPonto;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACBrPonto, ComCtrls, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    ACBrPonto: TACBrPonto;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    Button1: TButton;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    Memo2: TMemo;
    Button2: TButton;
    LabeledEdit6: TLabeledEdit;
    LabeledEdit7: TLabeledEdit;
    LabeledEdit8: TLabeledEdit;
    LabeledEdit9: TLabeledEdit;
    LabeledEdit10: TLabeledEdit;
    Memo3: TMemo;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialog.Execute then
  begin
    ACBrPonto.Ponto_AFD := ACBrPonto.ProcessarArquivo_AFD(OpenDialog.FileName);

    //Registro 1 - Cabecalho
    Memo1.Lines.Add('============ CABECALHO ============');
    Memo1.Lines.Add('Campo 01 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo01);
    Memo1.Lines.Add('Campo 02 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo02);
    Memo1.Lines.Add('Campo 03 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo03);
    Memo1.Lines.Add('Campo 04 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo04);
    Memo1.Lines.Add('Campo 05 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo05);
    Memo1.Lines.Add('Campo 06 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo06);
    Memo1.Lines.Add('Campo 07 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo07);
    Memo1.Lines.Add('Campo 08 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo08);
    Memo1.Lines.Add('Campo 09 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo09);
    Memo1.Lines.Add('Campo 10 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo10);
    Memo1.Lines.Add('Campo 11 = ' + ACBrPonto.Ponto_AFD.Cabecalho.Campo11);

    //Registro 2 - Registro de inclusão ou alteração da identificação da empresa no REP
    Memo1.Lines.Add('============ EMPRESA ============');
    Memo1.Lines.Add('Campo 01 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo01);
    Memo1.Lines.Add('Campo 02 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo02);
    Memo1.Lines.Add('Campo 03 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo03);
    Memo1.Lines.Add('Campo 04 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo04);
    Memo1.Lines.Add('Campo 05 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo05);
    Memo1.Lines.Add('Campo 06 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo06);
    Memo1.Lines.Add('Campo 07 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo07);
    Memo1.Lines.Add('Campo 08 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo08);
    Memo1.Lines.Add('Campo 09 = ' + ACBrPonto.Ponto_AFD.Registro2.Campo09);

    //Registro 3 - Registro de marcação de ponto
    Memo1.Lines.Add('============ MARCACAO ============');
    for i := 0 to ACBrPonto.Ponto_AFD.Registro3.Count - 1 do
    begin
      Memo1.Lines.Add('----------');
      Memo1.Lines.Add('Campo 01['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro3.Items[i].Campo01);
      Memo1.Lines.Add('Campo 02['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro3.Items[i].Campo02);
      Memo1.Lines.Add('Campo 03['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro3.Items[i].Campo03);
      Memo1.Lines.Add('Campo 04['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro3.Items[i].Campo04);
      Memo1.Lines.Add('Campo 05['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro3.Items[i].Campo05);
    end;

    //Registro 4 - Registro de ajuste do relógio de tempo real do REP
    Memo1.Lines.Add('============ AJUSTE RELOGIO ============');
    for i := 0 to ACBrPonto.Ponto_AFD.Registro4.Count - 1 do
    begin
      Memo1.Lines.Add('----------');
      Memo1.Lines.Add('Campo 01['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro4.Items[i].Campo01);
      Memo1.Lines.Add('Campo 02['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro4.Items[i].Campo02);
      Memo1.Lines.Add('Campo 03['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro4.Items[i].Campo03);
      Memo1.Lines.Add('Campo 04['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro4.Items[i].Campo04);
      Memo1.Lines.Add('Campo 05['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro4.Items[i].Campo05);
      Memo1.Lines.Add('Campo 06['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro4.Items[i].Campo06);
    end;

    //Registro 5 - Registro de inclusão ou alteração ou exclusão de empregado da MT do REP
    Memo1.Lines.Add('============ EMPREGADO ============');
    for i := 0 to ACBrPonto.Ponto_AFD.Registro5.Count - 1 do
    begin
      Memo1.Lines.Add('----------');
      Memo1.Lines.Add('Campo 01['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro5.Items[i].Campo01);
      Memo1.Lines.Add('Campo 02['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro5.Items[i].Campo02);
      Memo1.Lines.Add('Campo 03['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro5.Items[i].Campo03);
      Memo1.Lines.Add('Campo 04['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro5.Items[i].Campo04);
      Memo1.Lines.Add('Campo 05['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro5.Items[i].Campo05);
      Memo1.Lines.Add('Campo 06['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro5.Items[i].Campo06);
      Memo1.Lines.Add('Campo 07['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFD.Registro5.Items[i].Campo07);
    end;

    //Trailer
    Memo1.Lines.Add('============ TRAILER ============');
    Memo1.Lines.Add('Campo 01 = ' + ACBrPonto.Ponto_AFD.Trailer.Campo01);
    Memo1.Lines.Add('Campo 02 = ' + IntToStr(ACBrPonto.Ponto_AFD.Trailer.Campo02));
    Memo1.Lines.Add('Campo 03 = ' + IntToStr(ACBrPonto.Ponto_AFD.Trailer.Campo03));
    Memo1.Lines.Add('Campo 04 = ' + IntToStr(ACBrPonto.Ponto_AFD.Trailer.Campo04));
    Memo1.Lines.Add('Campo 05 = ' + IntToStr(ACBrPonto.Ponto_AFD.Trailer.Campo05));
    Memo1.Lines.Add('Campo 06 = ' + ACBrPonto.Ponto_AFD.Trailer.Campo06);

    Memo1.Lines.Add('==================================');
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  contador, i: Integer;
begin
  contador := 1;

  //Registro 1 - Cabecalho
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo01 := IntToStr(contador);
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo02 := '1';
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo03 := '1';
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo04 := LabeledEdit1.Text;
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo05 := LabeledEdit2.Text;
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo06 := LabeledEdit3.Text;
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo07 := LabeledEdit4.Text;
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo08 := LabeledEdit5.Text;
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo09 := LabeledEdit5.Text;
  ACBrPonto.Ponto_AFDT.Cabecalho.Campo10 := '1500';

  Memo2.Lines.Add('============ CABECALHO ============');
  Memo2.Lines.Add('Campo 01 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo01);
  Memo2.Lines.Add('Campo 02 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo02);
  Memo2.Lines.Add('Campo 03 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo03);
  Memo2.Lines.Add('Campo 04 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo04);
  Memo2.Lines.Add('Campo 05 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo05);
  Memo2.Lines.Add('Campo 06 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo06);
  Memo2.Lines.Add('Campo 07 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo07);
  Memo2.Lines.Add('Campo 08 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo08);
  Memo2.Lines.Add('Campo 09 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo09);
  Memo2.Lines.Add('Campo 10 = ' + ACBrPonto.Ponto_AFDT.Cabecalho.Campo10);

  //Registro 2 - Detalhe
  Memo2.Lines.Add('============ DETALHE ============');
  for i := 0 to 10 do
  begin
    inc(contador);
    with ACBrPonto.Ponto_AFDT.Registro2.New do
    begin
      Campo01 := IntToStr(contador);
      Campo02 := '2';
      Campo03 := LabeledEdit4.Text;
      if contador mod 2 > 0 then
        Campo04 := '0800'
      else
        Campo04 := '1200';
      Campo05 := IntToStr(i);
      Campo06 := 'NUMERO_RELOGIO';
      if contador mod 2 > 0 then
        Campo07 := 'E'
      else
        Campo07 := 'S';
      Campo09 := 'O';
    end;
    Memo2.Lines.Add('----------');
    Memo2.Lines.Add('Campo 01['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo01);
    Memo2.Lines.Add('Campo 02['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo02);
    Memo2.Lines.Add('Campo 03['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo03);
    Memo2.Lines.Add('Campo 04['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo04);
    Memo2.Lines.Add('Campo 05['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo05);
    Memo2.Lines.Add('Campo 06['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo06);
    Memo2.Lines.Add('Campo 07['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo07);
    Memo2.Lines.Add('Campo 08['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo08);
    Memo2.Lines.Add('Campo 09['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo09);
    Memo2.lines.Add('Campo 10['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_AFDT.Registro2.Items[i].Campo10);
  end;

  Memo2.Lines.Add('==================================');

  //Gera arquivo
  ACBrPonto.SaveFileTXT_AFDT('teste_afdt.txt');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  contador, i: Integer;
begin
  contador := 1;

  //Registro 1 - Cabecalho
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo01 := IntToStr(contador);
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo02 := '1';
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo03 := '1';
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo04 := LabeledEdit6.Text;
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo05 := LabeledEdit7.Text;
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo06 := LabeledEdit8.Text;
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo07 := LabeledEdit9.Text;
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo08 := LabeledEdit10.Text;
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo09 := LabeledEdit10.Text;
  ACBrPonto.Ponto_ACJEF.Cabecalho.Campo10 := '1400';

  Memo3.Lines.Add('============ CABECALHO ============');
  Memo3.Lines.Add('Campo 01 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo01);
  Memo3.Lines.Add('Campo 02 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo02);
  Memo3.Lines.Add('Campo 03 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo03);
  Memo3.Lines.Add('Campo 04 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo04);
  Memo3.Lines.Add('Campo 05 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo05);
  Memo3.Lines.Add('Campo 06 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo06);
  Memo3.Lines.Add('Campo 07 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo07);
  Memo3.Lines.Add('Campo 08 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo08);
  Memo3.Lines.Add('Campo 09 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo09);
  Memo3.Lines.Add('Campo 10 = ' + ACBrPonto.Ponto_ACJEF.Cabecalho.Campo10);

  //Registro 2 - Horários Contratuais
  Memo3.Lines.Add('============ HORARIOS ============');
  for i := 0 to 4 do
  begin
    inc(contador);
    with ACBrPonto.Ponto_ACJEF.Registro2.New do
    begin
      Campo01 := IntToStr(contador);
      Campo02 := '2';
      Campo03 := '0001';
      if contador mod 2 > 0 then
      begin
        Campo04 := '0800';
        Campo05 := '1200';
      end
      else
      begin
        Campo04 := '1400';
        Campo05 := '1800';
      end;
      Campo06 := '1200';
      Campo07 := '1400';
    end;
    Memo3.Lines.Add('----------');
    Memo3.Lines.Add('Campo 01['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo01);
    Memo3.Lines.Add('Campo 02['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo02);
    Memo3.Lines.Add('Campo 03['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo03);
    Memo3.Lines.Add('Campo 04['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo04);
    Memo3.Lines.Add('Campo 05['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo05);
    Memo3.Lines.Add('Campo 06['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo06);
    Memo3.Lines.Add('Campo 07['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo07);
  end;

  //Registro 3 - Detalhe
  Memo3.Lines.Add('============ DETALHE ============');
  for i := 0 to 4 do
  begin
    inc(contador);
    with ACBrPonto.Ponto_ACJEF.Registro3.New do
    begin
      Campo01 := IntToStr(contador);
      Campo02 := '3';
      Campo03 := IntToStr(i);
      Campo04 := LabeledEdit9.Text;
      Campo05 := '0800';
      Campo06 := '0001';
    end;
    Memo3.Lines.Add('----------');
    Memo3.Lines.Add('Campo 01['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo01);
    Memo3.Lines.Add('Campo 02['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo02);
    Memo3.Lines.Add('Campo 03['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo03);
    Memo3.Lines.Add('Campo 04['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo04);
    Memo3.Lines.Add('Campo 05['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo05);
    Memo3.Lines.Add('Campo 06['+IntToStr(i)+'] = ' + ACBrPonto.Ponto_ACJEF.Registro2.Items[i].Campo06);
  end;

  Memo3.Lines.Add('==================================');

  //Gera arquivo
  ACBrPonto.SaveFileTXT_ACJEF('teste_acjef.txt');
end;

end.
