unit uPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, StdCtrls, Forms,
  Dialogs, Buttons, ACBrBase, ACBrDFe, ACBrBlocoX;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ACBrBlocoX1: TACBrBlocoX;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ACBrBlocoX_Comum;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  if SaveDialog1.Execute then
  begin
    with ACBrBlocoX1 do
    begin
      Configuracoes.Certificados.NumeroSerie := Edit1.Text;
      Configuracoes.Certificados.Senha       := Edit2.Text;

      Estabelecimento.Ie              := '123456789';
      Estabelecimento.Cnpj            := '99999999999999';
      Estabelecimento.NomeEmpresarial := 'NOME EMPRESARIAL';

      PafECF.Versao                       := '01.01.01';
      PafECF.NumeroCredenciamento         := '123456ABC';
      PafECF.NomeComercial                := 'NOME COMERCIAL';
      PafECF.NomeEmpresarialDesenvolvedor := 'NOME EMPRESARIAL DO DESENVOLVEDOR';
      PafECF.CnpjDesenvolvedor            := '88888888888888';
    end;

    // arquivo de Estoque
    for I := 1 to 10 do
    begin
      ACBrBlocoX1.Estoque.DataReferenciaInicial := DATE;
      ACBrBlocoX1.Estoque.DataReferenciaFinal   := DATE;

      with ACBrBlocoX1.Estoque.Add do
      begin
        Codigo.Tipo             := tpcGTIN;
        Codigo.Numero           := '7891234567891';
        Descricao               := 'PRODUTO TESTE ' + IntToStr(I);
        ValorUnitario           := 1.23;
        Ippt                    := ipptTerceiros;
        SituacaoTributaria      := stTributado;
        Aliquota                := 12;
        Unidade                 := 'UN';
        Quantidade              := 1234;
        IndicadorArredondamento := False;
      end;
    end;

    ACBrBlocoX1.Estoque.SaveToFile(SaveDialog1.FileName);
    ShowMessage('terminado');
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  if SaveDialog1.Execute then
  begin
    with ACBrBlocoX1 do
    begin
      Configuracoes.Certificados.NumeroSerie := Edit1.Text;
      Configuracoes.Certificados.Senha       := Edit2.Text;

      Estabelecimento.Ie              := '123456789';
      Estabelecimento.Cnpj            := '99999999999999';
      Estabelecimento.NomeEmpresarial := 'NOME EMPRESARIAL';

      PafECF.Versao                       := '01.01.01';
      PafECF.NumeroCredenciamento         := '123456ABC';
      PafECF.NomeComercial                := 'NOME COMERCIAL';
      PafECF.NomeEmpresarialDesenvolvedor := 'NOME EMPRESARIAL DO DESENVOLVEDOR';
      PafECF.CnpjDesenvolvedor            := '88888888888888';

      ECF.NumeroFabricacao := 'BR1234567891234579';
      ECF.Tipo             := 'ECF-IF';
      ECF.Marca            := 'MARCA ECF';
      ECF.Modelo           := 'MODELO ECF';
      ECF.Versao           := '010101';
    end;

    // arquivo de Estoque
    for I := 0 to 10 do
    begin


    end;

    ACBrBlocoX1.ReducoesZ.SaveToFile(SaveDialog1.FileName);
    ShowMessage('terminado');
  end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Edit1.Text := ACBrBlocoX1.SSL.SelecionarCertificado;
end;

end.
