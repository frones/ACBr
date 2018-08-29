unit uPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, StdCtrls, Forms,
  Dialogs, Buttons, ACBrBase, ACBrDFe, ACBrBlocoX;

type
  TfrmPrincipal = class(TForm)
    Button1: TButton;
    ACBrBlocoX1: TACBrBlocoX;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure PreencherCabecalho(const AACBrBlocoX: TACBrBlocoX);
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  ACBrBlocoX_Comum;

{$R *.dfm}

procedure TfrmPrincipal.PreencherCabecalho(const AACBrBlocoX: TACBrBlocoX);
begin
  with AACBrBlocoX do
  begin
    Configuracoes.Certificados.NumeroSerie := Edit1.Text;
    Configuracoes.Certificados.Senha       := Edit2.Text;

    Estabelecimento.Ie              := '123456789';
    Estabelecimento.Cnpj            := '99999999999999';
    Estabelecimento.NomeEmpresarial := 'NOME EMPRESARIAL';

    PafECF.Versao                       := '01.01.01';
    PafECF.NumeroCredenciamento         := '123456789012345';
    PafECF.NomeComercial                := 'NOME COMERCIAL';
    PafECF.NomeEmpresarialDesenvolvedor := 'NOME EMPRESARIAL DO DESENVOLVEDOR';
    PafECF.CnpjDesenvolvedor            := '88888888888888';
  end;
end;

procedure TfrmPrincipal.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  if SaveDialog1.Execute then
  begin
    with ACBrBlocoX1 do
    begin
      PreencherCabecalho(ACBrBlocoX1);

      // arquivo de Estoque
      with Estoque do
      begin
        DataReferencia := DATE;

        Produtos.Clear;
        for I := 1 to 10 do
        begin
          with Produtos.Add do
          begin
            Codigo.Tipo             := tpcGTIN;
            Codigo.CodigoGTIN       := '7891234567891';
            Codigo.CodigoNCMSH      := '11041900';
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

        SaveToFile(SaveDialog1.FileName);
        ShowMessage('terminado');
      end;
    end;
  end;
end;

procedure TfrmPrincipal.Button2Click(Sender: TObject);
var
  I, X: Integer;
begin
  if SaveDialog1.Execute then
  begin
    with ACBrBlocoX1 do
    begin
      PreencherCabecalho(ACBrBlocoX1);

      ECF.NumeroFabricacao := 'BR1234567891234579';
      ECF.Tipo             := 'ECF-IF';
      ECF.Marca            := 'MARCA ECF';
      ECF.Modelo           := 'MODELO ECF';
      ECF.Versao           := '010101';

      with ReducoesZ do
      begin
        ReducoesZ.DataReferencia   := DATE;
        ReducoesZ.CRZ              := 1;
        ReducoesZ.COO              := 1;
        ReducoesZ.CRO              := 1;
        ReducoesZ.VendaBrutaDiaria := 3456.78;
        ReducoesZ.GT               := 123456789.45;

        TotalizadoresParciais.Clear;
        for I := 1 to 2 do
        begin
          with TotalizadoresParciais.Add do
          begin
            Identificacao := '00T1234';
            Valor       := 1234.56;

            for X := 1 to 2 do
            begin
              with Produtos.Add do
              begin
                Codigo.Tipo   := tpcProprio;
                Codigo.CodigoProprio := IntToStr(X);
                Codigo.CodigoNCMSH := '11041900';
                Descricao     := 'PRODUTO ' + IntToStr(X);
                Quantidade    := 1234556;
                Unidade       := 'UN';
                ValorUnitario := 1234.99;
              end;
            end;

            for X := 1 to 2 do
            begin
              with Servicos.Add do
              begin
                Codigo.Tipo   := tpcProprio;
                Codigo.CodigoProprio := IntToStr(X);
                Codigo.CodigoNCMSH := '11041900';
                Descricao     := 'SERVICO ' + IntToStr(X);
                Quantidade    := 1234556;
                Unidade       := 'UN';
                ValorUnitario := 1234.99;
              end;
            end;
          end;
        end;

        SaveToFile(SaveDialog1.FileName);
        ShowMessage('terminado');
      end;
    end;
  end;
end;

procedure TfrmPrincipal.Button3Click(Sender: TObject);
var
  RespostaValidacao: String;
  Arquivo: TStringList;
begin
  if OpenDialog1.Execute then
  begin
    Arquivo := TStringList.Create;
    try
      Arquivo.LoadFromFile(OpenDialog1.FileName);

      ACBrBlocoX1.WebServices.ValidarBlocoX.Clear;
      ACBrBlocoX1.WebServices.ValidarBlocoX.XML := Arquivo.Text;

      ACBrBlocoX1.WebServices.ValidarBlocoX.Executar;
      RespostaValidacao := ACBrBlocoX1.WebServices.ValidarBlocoX.RetWS;

      if Pos('VALIDADO COM SUCESSO', UpperCase(RespostaValidacao)) > 0 then
      begin
        ShowMessage(RespostaValidacao);

        ACBrBlocoX1.WebServices.EnviarBlocoX.Clear;
        ACBrBlocoX1.WebServices.EnviarBlocoX.XML := Arquivo.Text;

        if ACBrBlocoX1.WebServices.EnviarBlocoX.Executar then
        begin
          ShowMessage(
            'Arquivo enviado com sucesso!' +
            sLineBreak +
            sLineBreak +
            ACBrBlocoX1.WebServices.EnviarBlocoX.RetWS
          );
        end
        else
        begin
          raise Exception.Create(
            'Não foi possível transmitir o arquivo!' + sLineBreak +
            ACBrBlocoX1.WebServices.EnviarBlocoX.RetWS
          );
        end;
      end
      else
        raise Exception.Create(RespostaValidacao);
    finally
      Arquivo.Free;
    end;
  end;
end;

procedure TfrmPrincipal.SpeedButton1Click(Sender: TObject);
begin
  Edit1.Text := ACBrBlocoX1.SSL.SelecionarCertificado;
end;

end.
