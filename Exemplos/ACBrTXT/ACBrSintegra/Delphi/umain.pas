unit umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, CheckLst, Mask, ACBrSintegra, ExtCtrls;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    Edit1: TEdit;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    GroupBox4: TGroupBox;
    CheckListBox1: TCheckListBox;
    MaskEdit1: TMaskEdit;
    MaskEdit2: TMaskEdit;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Edit3: TEdit;
    ACBrSintegra: TACBrSintegra;
    Panel1: TPanel;
    Label9: TLabel;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label10: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure GerarRegistro10;
    procedure GerarRegistro11;
    procedure GerarRegistro50;
    procedure GerarRegistro51;
    procedure GerarRegistro53;
    procedure GerarRegistro54;
    procedure GerarRegistro60M;
    procedure GerarRegistro60A;
    procedure GerarRegistro60D;
    procedure GerarRegistro70;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
if Edit1.text='' then begin
   messagebox(0,'Informe o nome do responsável','Informação',mb_iconinformation+mb_taskmodal);
   Edit1.setfocus;
   exit;
end;
try
  if StrToDate(MaskEdit2.text)<StrToDate(MaskEdit1.Text) then begin
     application.MessageBox('Data Final não pode ser menor que a inicial','Erro',mb_iconstop);
     MaskEdit2.setfocus;
     exit;
     end
   else if StrToDate(MaskEdit1.text)>date then begin
     application.MessageBox('Data Final não pode ser maior que a data atual','Erro',mb_iconstop);
     MaskEdit2.setfocus;
     exit;
  end;
except
  application.MessageBox('Intervalo de datas inválido.','Erro',mb_iconstop);
  MaskEdit1.setfocus;
  exit;
end;
SaveDialog1.InitialDir:=ExtractFilePath(Application.ExeName);
if ComboBox4.ItemIndex=0 then
  SaveDialog1.FileName:='ArquivoSintegraVCL';

if SaveDialog1.Execute then
begin
  try
    try
      if ComboBox4.ItemIndex=0 then
      begin
        ACBrSintegra.FileName:=SaveDialog1.FileName;
        ACBrSintegra.VersaoValidador:=TVersaoValidador(ComboBox5.ItemIndex);
        GerarRegistro10;
        GerarRegistro11;
        if CheckListBox1.Checked[0] then
          GerarRegistro50;
        if CheckListBox1.Checked[1] then
          GerarRegistro51;
        if CheckListBox1.Checked[2] then
          GerarRegistro53;
        if CheckListBox1.Checked[3] then
          GerarRegistro54;
        if CheckListBox1.Checked[4] then
          GerarRegistro60M;
        if CheckListBox1.Checked[5] then
          GerarRegistro60A;
        if CheckListBox1.Checked[6] then
          GerarRegistro60D;
        if CheckListBox1.Checked[7] then
          GerarRegistro70;
        ACBrSintegra.GeraArquivo;
      end;
      MessageBox(0,'Geração concluída','Informação',mb_iconinformation+mb_taskmodal);
    except
      on e:Exception do
      begin
        Application.MessageBox(PChar('Erro ao gerar arquivo do sintegra!'+#13+
          'Erro: '+e.Message),'Erro',MB_ICONSTOP+MB_TASKMODAL);
      end;
    end;
  finally
  end;
end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
if ComboBox4.ItemIndex=0 then
begin
  ShowMessage(ACBrSintegra.Versao);
end
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
CheckListBox1.Checked[0]:=True;
CheckListBox1.Checked[2]:=True;
CheckListBox1.Checked[3]:=True;
CheckListBox1.Checked[4]:=True;
CheckListBox1.Checked[5]:=True;
CheckListBox1.Checked[6]:=True;
end;

procedure TForm1.GerarRegistro10;
begin
with ACBrSintegra do
begin
  Registro10.CNPJ := Edit2.Text;
  Registro10.Inscricao := Edit3.Text;
  Registro10.RazaoSocial := 'EMPRESA DE TESTE';
  Registro10.Cidade := 'CIDADE DE TESTE';
  Registro10.Estado := 'MG';
  Registro10.Telefone := '3511111111';
  Registro10.DataInicial := StrToDate(MaskEdit1.Text);
  Registro10.DataFinal := StrToDate(MaskEdit2.Text);
  Registro10.CodigoConvenio := IntToStr(ComboBox1.ItemIndex + 1);
  Registro10.NaturezaInformacoes := IntToStr(ComboBox2.ItemIndex + 1);
  Registro10.FinalidadeArquivo := Copy(ComboBox3.Items[ComboBox3.ItemIndex], 1, 1);
end;
end;

procedure TForm1.GerarRegistro11;
begin
with ACBrSintegra do
begin
  Registro11.Endereco:='ENDERECO DA EMPRESA';
  Registro11.Numero:='1';
  Registro11.Bairro:='BAIRRO DA EMPRESA';
  Registro11.Cep:='11.111-111';
  Registro11.Responsavel:='RESPONSAVEL';
  Registro11.Telefone:='1111111111';
end;
end;

procedure TForm1.GerarRegistro50;
var
  wregistro50: TRegistro50;
begin
  wregistro50:=TRegistro50.Create;
  with ACBrSintegra do
  begin
    wregistro50.CPFCNPJ:='43.214.055/0042-85';
    wregistro50.Inscricao:='349016872110';
    wregistro50.DataDocumento:=Registro10.DataFinal;
    wregistro50.UF:='SP';
    wregistro50.Modelo:='1';
    wregistro50.Serie:='1';
    wregistro50.Numero:='123';
    wregistro50.Cfop:='2.102';
    wregistro50.EmissorDocumento:='T';
    wregistro50.ValorContabil:=1500;
    wregistro50.BasedeCalculo:=1500;
    wregistro50.Icms:=270;
    wregistro50.Isentas:=0;
    wregistro50.Outras:=0;
    wregistro50.Aliquota:=18;
    wregistro50.Situacao:='N';
    ACBrSintegra.Registros50.Add(wregistro50);
  end;
end;

procedure TForm1.GerarRegistro51;
begin

end;

procedure TForm1.GerarRegistro53;
begin

end;

procedure TForm1.GerarRegistro54;
var
  wregistro54: TRegistro54;
  wregistro75: TRegistro75;
begin
wregistro54:=TRegistro54.Create;
wregistro54.CPFCNPJ:='43.214.055/0042-85';
wregistro54.Modelo:='1';
wregistro54.Serie:='1';
wregistro54.Numero:='123';
wregistro54.Cfop:='2.102';
wregistro54.CST:='000';
wregistro54.NumeroItem:=1;
wregistro54.Codigo:='0000000000017';
wregistro54.Descricao:='PRODUTO DE TESTE';
wregistro54.Quantidade:=50;
wregistro54.Valor:=1500;
wregistro54.ValorDescontoDespesa:=0;
wregistro54.BasedeCalculo:=1500;
wregistro54.BaseST:=0;
wregistro54.ValorIpi:=0;
wregistro54.Aliquota:=18;
ACBrSintegra.Registros54.Add(wregistro54);

wregistro75:=TRegistro75.Create;
wregistro75.Codigo:=wregistro54.Codigo;
wregistro75.AliquotaICMS:=wregistro54.Aliquota;
wregistro75.DataInicial:=ACBrSintegra.Registro10.DataInicial;
wregistro75.DataFinal:=ACBrSintegra.Registro10.DataFinal;
wregistro75.Descricao:='PRODUTO DE TESTE';
wregistro75.Unidade:='UN';
ACBrSintegra.Registros75.Add(wregistro75);

end;

procedure TForm1.GerarRegistro60A;
var
  wregistro60A: TRegistro60A;
begin
wregistro60A:=TRegistro60A.Create;
wregistro60A.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60A.NumSerie:='000000987456';
wregistro60A.StAliquota:='F';
wregistro60A.Valor:=500;
ACBrSintegra.Registros60A.Add(wregistro60A);

wregistro60A:=TRegistro60A.Create;
wregistro60A.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60A.NumSerie:='000000987456';
wregistro60A.StAliquota:='I';
wregistro60A.Valor:=550;
ACBrSintegra.Registros60A.Add(wregistro60A);

wregistro60A:=TRegistro60A.Create;
wregistro60A.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60A.NumSerie:='000000987456';
wregistro60A.StAliquota:='1800';
wregistro60A.Valor:=1000.35;
ACBrSintegra.Registros60A.Add(wregistro60A);

wregistro60A:=TRegistro60A.Create;
wregistro60A.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60A.NumSerie:='000000777777';
wregistro60A.StAliquota:='F';
wregistro60A.Valor:=100;
ACBrSintegra.Registros60A.Add(wregistro60A);

wregistro60A:=TRegistro60A.Create;
wregistro60A.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60A.NumSerie:='000000777777';
wregistro60A.StAliquota:='I';
wregistro60A.Valor:=100;
ACBrSintegra.Registros60A.Add(wregistro60A);

wregistro60A:=TRegistro60A.Create;
wregistro60A.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60A.NumSerie:='000000777777';
wregistro60A.StAliquota:='1800';
wregistro60A.Valor:=100;
ACBrSintegra.Registros60A.Add(wregistro60A);

end;

procedure TForm1.GerarRegistro60D;
var
  wregistro60D: TRegistro60D;
  wregistro75: TRegistro75;
begin
wregistro60D:=TRegistro60D.Create;

wregistro60D.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60D.NumSerie:='000000987456';
wregistro60D.Codigo:='1234567890123';
wregistro60D.Quantidade:=100;
wregistro60D.Valor:=500;
wregistro60D.BaseDeCalculo:=0;
wregistro60D.StAliquota:='F';
wregistro60D.ValorIcms:=0;
ACBrSintegra.Registros60D.Add(wregistro60D);

wregistro75:=TRegistro75.Create;
wregistro75.Codigo:=wregistro60D.Codigo;
wregistro75.AliquotaICMS:=0;
wregistro75.DataInicial:=ACBrSintegra.Registro10.DataInicial;
wregistro75.DataFinal:=ACBrSintegra.Registro10.DataFinal;
wregistro75.Descricao:='PRODUTO DE TESTE';
wregistro75.Unidade:='UN';
ACBrSintegra.Registros75.Add(wregistro75);


wregistro60D:=TRegistro60D.Create;
wregistro60D.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60D.NumSerie:='000000777777';
wregistro60D.Codigo:='0000000000017';
wregistro60D.Quantidade:=100;
wregistro60D.Valor:=550;
wregistro60D.BaseDeCalculo:=0;
wregistro60D.StAliquota:='I';
wregistro60D.ValorIcms:=0;
ACBrSintegra.Registros60D.Add(wregistro60D);

wregistro75:=TRegistro75.Create;
wregistro75.Codigo:=wregistro60D.Codigo;
wregistro75.AliquotaICMS:=0;
wregistro75.DataInicial:=ACBrSintegra.Registro10.DataInicial;
wregistro75.DataFinal:=ACBrSintegra.Registro10.DataFinal;
wregistro75.Descricao:='PRODUTO DE TESTE II';
wregistro75.Unidade:='UN';
ACBrSintegra.Registros75.Add(wregistro75);

end;

procedure TForm1.GerarRegistro60M;
var
  wregistro60M: TRegistro60M;
begin
wregistro60M:=TRegistro60M.Create;
wregistro60M.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60M.NumSerie:='000000987456';
wregistro60M.NumOrdem:=1;
wregistro60M.ModeloDoc:='2D';
wregistro60M.CooInicial:=1000;
wregistro60M.CooFinal:=1050;
wregistro60M.CRZ:=1;
wregistro60M.CRO:=1;
wregistro60M.VendaBruta:=2050.35;
wregistro60M.ValorGT:=10000;
ACBrSintegra.Registros60M.Add(wregistro60M);

wregistro60M:=TRegistro60M.Create;
wregistro60M.Emissao:=ACBrSintegra.Registro10.DataInicial;
wregistro60M.NumSerie:='000000777777';
wregistro60M.NumOrdem:=2;
wregistro60M.ModeloDoc:='2D';
wregistro60M.CooInicial:=2000;
wregistro60M.CooFinal:=2010;
wregistro60M.CRZ:=1;
wregistro60M.CRO:=1;
wregistro60M.VendaBruta:=300;
wregistro60M.ValorGT:=15000;
ACBrSintegra.Registros60M.Add(wregistro60M);

end;

procedure TForm1.GerarRegistro70;
begin

end;


end.
