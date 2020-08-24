unit ufrmPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ACBrDANFCeFortesFrA4, ACBrADRCST;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrADRCST1: TACBrADRCST;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses ACBrADRCSTConversao;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  ip : Integer;
  ine : integer;
begin
  ACBrADRCST1.Path := ExtractFilePath(ParamStr(0));
  ACBrADRCST1.Arquivo := 'NomeDoArquivo.txt';

  ACBrADRCST1.Delimitador := '|';
  ACBrADRCST1.CurMascara := '#0.00';
  ACBrADRCST1.IniciaGeracao;

  with ACBrADRCST1.Bloco_0.Registro0000New do
  begin
    COD_VERSAO := vlADRCSTVersao100;
    MES_ANO := EncodeDate(2020, 01, 01);
    CNPJ := '00000000000100';
    IE := '0123456789';
    NOME := 'Nome Empresarial até 100';
    CD_FIN := afnArquivoOriginal;
    N_REG_ESPECIAL := '';//se possuir
    CNPJ_CD := '';//se possuir
    IE_CD := '';// se possui
  end;

  for ip := 1 to 10 do
  begin
    with ACBrADRCST1.Bloco_1.Registro1000New do
    begin
      if (ip mod 2)=0 then
        IND_FECOP := indProdutoNaoSujeitoFECOP
      else
        IND_FECOP := indProdutoSujeitoFECOP;


      COD_ITEM := IntToStr(ip);
      COD_BARRAS := '01234567890123';
      COD_ANP := '123456789';
      NCM := '12345678';
      CEST := '1234567';
      DESCR_ITEM := 'PRODUTO '+ IntToStr(ip);
      UNID_ITEM := 'UN';
      ALIQ_ICMS_ITEM := 17.00;
      ALIQ_FECOP := 1.23;
      QTD_TOT_ENTRADA := 123456.789;
      QTD_TOT_SAIDA := 123456.789;

      // Registro1010 criar
      if (ip mod 2)=0 then
        with Registro1010New do
        begin
          QTD:= Random(300);
          VL_TOT_ITEM:= Random(13213);
          TXT_COMPL:= 'texto';
        end;

      with Registro1100 do
      begin
        QTD_TOT_ENTRADA:= Random(200);
        MENOR_VL_UNIT_ITEM:= Random(10);
        VL_BC_ICMSST_UNIT_MED:= Random(300);
        VL_TOT_ICMS_SUPORT_ENTR:= Random(540);
        VL_UNIT_MED_ICMS_SUPORT_ENTR:= Random(16556);


        for ine := 0 to 5 do
          with Registro1110List.New() do
          begin
            DT_DOC:=now;
            COD_RESP_RET:=inRRProprioDeclarante;
            CST_CSOSN:=60;
            CHAVE:='01234567890123456789012345678901234567890123';
            N_NF:= ine;
            CNPJ_EMIT:= '01234567890123';
            UF_EMIT:= 'XX';
            CNPJ_DEST:= '01234567890123';
            UF_DEST:= 'XX';
            CFOP:=1234;
            N_ITEM:=1;
            //UNID_ITEM //não inform pega do 1100 já
            QTD_ENTRADA:= random(100);
            VL_UNIT_ITEM:=random(999);
            VL_BC_ICMS_ST:= random(999);
            VL_ICMS_SUPORT_ENTR:= random(99999);
          end;

        for ine := 0 to 2 do
          with Registro1120List.New() do
          begin
            DT_DOC:=now;
            CST_CSOSN:=60;
            CHAVE:='01234567890123456789012345678901234567890123';
            N_NF:= ine;
            CNPJ_EMIT:= '01234567890123';
            UF_EMIT:= 'XX';
            CNPJ_DEST:= '01234567890123';
            UF_DEST:= 'XX';
            CFOP:=1234;
            N_ITEM:=1;
            //UNID_ITEM //não inform pega do 1100 já
            QTD_DEVOLVIDA:= random(100);
            VL_UNIT_ITEM:=random(999);
            VL_BC_ICMS_ST:= random(999);
            VL_ICMS_SUPORT_ENTR:= random(99999);
            CHAVE_REF:= '000000000000000000000000000000000000000';
            N_ITEM_REF:= 99;
          end;
      end;
      //with Registro1200New do
      //begin
      //  QTD_TOT_SAIDA:= 100;
      //  VL_TOT_ICMS_EFETIVO:= 99;
      //  VL_CONFRONTO_ICMS_ENTRADA:= 99;
      //  RESULT_RECUPERAR_RESSARCIR:= 99;
      //  RESULT_COMPLEMENTAR:=99;
      //  APUR_ICMSST_RECUPERAR_RESSARCIR:=0;
      //  APUR_ICMSST_COMPLEMENTAR:=0;
      //  APUR_FECOP_RESSARCIR:=0;
      //  APUR_FECOP_COMPLEMENTAR:=99;
      //
      //  for ine := 0 to 10 do
      //  begin
      //    //with Registro1210List.New do
      //    //begin
      //    //  DT_DOC: string read FDT_DOC write FDT_DOC;
      //    //  CST_CSOSN: integer read FCST_CSOSN write FCST_CSOSN;
      //    //  CHAVE: string read FCHAVE write FCHAVE;
      //    //  N_NF: integer read FN_NF write FN_NF;
      //    //  CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
      //    //  UF_EMIT: string read FUF_EMIT write FUF_EMIT;
      //    //  CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
      //    //  UF_DEST: string read FUF_DEST write FUF_DEST;
      //    //  CFOP: integer read FCFOP write FCFOP;
      //    //  N_ITEM: integer read FN_ITEM write FN_ITEM;
      //    //  //UNID_ITEM: string read FUNID_ITEM ;
      //    //  QTD_SAIDA: Double read FQTD_SAIDA write FQTD_SAIDA;
      //    //  VL_UNIT_ITEM: Double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
      //    //  VL_ICMS_EFETIVO: Double read FVL_ICMS_EFETIVO write FVL_ICMS_EFETIVO;
      //    //end;
      //  end;
      //end;
    end;

  end;


  ACBrADRCST1.SaveFileTXT;
end;

end.

