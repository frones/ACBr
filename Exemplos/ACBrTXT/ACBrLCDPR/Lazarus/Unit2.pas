unit Unit2;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Registro9999, BlocoQ,
  Bloco0050, Bloco0040, Registro0030, Registro0010, Registro0000, LCDPRUtils, LCDPRBlocos,
  uDadosContador, UACBrLCDPR;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    LCDPR1: TACBrLCDPR;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  i : Integer;
begin
  with LCDPR1 do
    begin
      Path := ExtractFilePath(Application.ExeName);

      try
        with Bloco0000 do
          begin
            COD_VER := Versao011;
            CPF := '000.000.000-00';
            NOME := 'Willian Aparecido Hübner do Nascimento';
            IND_SIT_INI_PER := indRegular;
            DT_SIT_ESP := Date;
            DT_INI := StrToDate('01/01/2019');
            DT_FIN := StrToDate('31/12/2019');
          end;

        with Bloco0010 do
          begin
            FORMA_APUR := faLivroCaixa;
          end;

        with Bloco0030 do
          begin
            ENDERECO := 'Rua das Mangueiras';
            NUM := '177';
            COMPL := 'Casa Vermelha';
            BAIRRO := 'Jardim Jacarandás';
            UF := 'MT';
            COD_MUN := '5107909';
            CEP := '78557679';
            NUM_TEL := '66999554613';
            EMAIL := 'willian.hubner@hotmail.co.uk';
          end;

        for I := 1 to 3 do
          begin
            with Bloco0040.Bloco0040New do
              begin
                with Bloco0040 do
                  begin
                    COD_IMOVEL := I;
                    PAIS := 'BRA';
                    MOEDA := 'BRL';
                    CAD_ITR := 12345678;
                    CAEPF := '123456789012';
                    INSCR_ESTADUAL := '00000000-0';
                    NOME_IMOVEL := 'BRASIL SISTEMAS ' + IntToStr(I);
                    ENDERECO := 'Rua das Primaveras';
                    NUM := '1538';
                    COMPL := 'Esquina com Pariris';
                    BAIRRO := 'Jardim Jacarandás';
                    UF := 'MT';
                    COD_MUN := '5107909';
                    CEP := '78557679';
                    TIPO_EXPLORACAO := teExploracaoInd;
                    PARTICIPACAO := 90;
                  end;

                  with Registro0045New do
                    begin
                      COD_IMOVEL := I;
                      TIPO_CONTRAPARTE := tpcArrendante;
                      CPF_CONTRAPARTE := '000.000.000-00';
                      NOME_CONTRAPARTE := 'Willian Aparecido Hübner do Nascimento';
                      PERC_CONTRAPARTE := 10;
                    end;
              end;
          end;

        with Bloco0050.Registro0050New do
          begin
            COD_CONTA := 1;
            PAIS_CTA := 'BRA';
            BANCO := 001;
            NOME_BANCO := 'Banco do Brasil';
            AGENCIA := '1180';
            NUM_CONTA := '121231';
          end;

        with BlocoQ.RegistroQ100New do
          begin
            DATA := EncodeDate(2019, 1, 1); //JANEIRO
            COD_IMOVEL := 1;
            COD_CONTA := 1;
            NUM_DOC := 'DOCUMENTO 1';
            TIPO_DOC := tdNotaFiscal;
            HISTORICO := 'Compra referente a nota fiscal numero 0001';
            ID_PARTIC := '000.000.000-00';  // O validador não aceita sem esta informação nos casos reais
            TIPO_LANC := tlDespesaCusteio;
            VL_ENTRADA := 0;
            VL_SAIDA := 1000.84;
            SLD_FIN := -1000.84;
            if SLD_FIN < 0 then
               NAT_SLD_FIN := 'N'
            else
               NAT_SLD_FIN := 'P';
          end;

        with BlocoQ.RegistroQ100New do
          begin
            DATA := EncodeDate(2019, 1, 1); //JANEIRO
            COD_IMOVEL := 1;
            COD_CONTA := 1;
            NUM_DOC := 'DOCUMENTO 2';
            TIPO_DOC := tdNotaFiscal;
            HISTORICO := 'Compra referente a nota fiscal numero 0002';
            ID_PARTIC := '000.000.000-00';
            TIPO_LANC := tlDespesaCusteio;
            VL_ENTRADA := 0;
            VL_SAIDA := 500.79;
            SLD_FIN := -1501.63;
            if SLD_FIN < 0 then
               NAT_SLD_FIN := 'N'
            else
               NAT_SLD_FIN := 'P';
          end;

        with BlocoQ.RegistroQ100New do
          begin
            DATA := EncodeDate(2019, 2, 1); //FEVEREIRO
            COD_IMOVEL := 1;
            COD_CONTA := 1;
            NUM_DOC := 'DOCUMENTO 3';
            TIPO_DOC := tdNotaFiscal;
            HISTORICO := 'Venda referente a nota fiscal numero 0003';
            ID_PARTIC := '000.000.000-00';
            TIPO_LANC := tlReceitaRural;
            VL_ENTRADA := 16000.87;
            VL_SAIDA := 0;
            SLD_FIN := 14499.24;
            if SLD_FIN < 0 then
               NAT_SLD_FIN := 'N'
            else
               NAT_SLD_FIN := 'P';
          end;

        with BlocoQ.RegistroQ200New do
          begin
            MES := '012019';                //TOTALIZAÇÃO DE JANEIRO
            VL_ENTRADA := 0;
            VL_SAIDA := 1501.63;
            SLD_FIN := -1501.63;
            if SLD_FIN < 0 then
               NAT_SLD_FIN := 'N'
            else
               NAT_SLD_FIN := 'P';
          end;

        with BlocoQ.RegistroQ200New do
          begin
            MES := '022019';                //TOTALIZAÇÃO DE FEVEREIRO
            VL_ENTRADA := 16000.87;
            VL_SAIDA := 0;
            SLD_FIN := 14499.24;
            if SLD_FIN < 0 then
               NAT_SLD_FIN := 'N'
            else
               NAT_SLD_FIN := 'P';
          end;

        DadosContador.IDENT_NOME := 'Willian Hubner';
        DadosContador.IDENT_CPF_CNPJ := '0000000000';
        DadosContador.IND_CRC := '000000000';
        DadosContador.EMAIL := 'email@emitir.com.br';
        DadosContador.FONE := '0000000000';

      except
        on e : exception do
          Memo1.Lines.Add(e.Message);
      end;

      PrepararArquivo;
      GerarBlocos;
      SalvarBlocos;
    end;
end;

end.
