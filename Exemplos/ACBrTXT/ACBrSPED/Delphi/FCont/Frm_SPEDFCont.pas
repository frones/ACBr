unit Frm_SPEDFCont;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ACBrUtil, ACBrTXTClass,
  ACBrSpedFCont;

type

  { TFrmSPEDFCont }

  TFrmSPEDFCont = class(TForm)
     btnB_0: TButton;
    btnB_I: TButton;
    btnB_J: TButton;
    btnError: TButton;
    btnTXT: TButton;
    memoError: TMemo;
    edtFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    memoTXT: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    btnB_M: TButton;
    ACBrSPEDFCont1: TACBrSPEDFCont;
    procedure btnB_0Click(Sender: TObject);
    procedure btnB_IClick(Sender: TObject);
    procedure btnB_JClick(Sender: TObject);
    procedure btnErrorClick(Sender: TObject);
    procedure btnTXTClick(Sender: TObject);
    procedure btnB_MClick(Sender: TObject);
    procedure ACBrSPEDFCont1Error(const MsnError: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSPEDFCont: TFrmSPEDFCont;

implementation

uses ACBrFContBloco_M_Class, ACBrFContBloco_I_Class, ACBrFContBloco_I;

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}


procedure TFrmSPEDFCont.btnB_0Click(Sender: TObject);
begin
   btnB_0.Enabled := false;

   ACBrSPEDFCont1.Bloco_0.LimpaRegistros;
   with ACBrSPEDFCont1.Bloco_0 do
   begin

      // Bloco 0 - Identificação e referências
      with Registro0000 do
      begin
         DT_INI  := StrToDate('01/01/2010');
         DT_FIN  := StrToDate('31/12/2010');
         NOME    := 'ESTABELECIMENTO TESTE DO FCONT';
         CNPJ    := '00910509000171';
         UF      := 'SP';
         IE      := '5350546531180';
         COD_MUN := '3550308';
         IM      := '111111';
         IND_SIT_ESP := '';
      end;

   end;
end;

procedure TFrmSPEDFCont.btnB_IClick(Sender: TObject);
begin
   btnB_I.Enabled := false;

   ACBrSPEDFCont1.Bloco_I.LimpaRegistros;

   // Bloco I - Lançamentos Contábeis
   with ACBrSPEDFCont1.Bloco_I do
   begin
      RegistroI001.IND_DAD := 0;

      with RegistroI050.New do
      begin
         DT_ALT := StrToDate('01/01/2010');
         COD_NAT := '01';
         IND_CTA := 'S';
         NIVEL := '1';
         COD_CTA := '2328A';
         COD_CTA_SUP := '';
         CTA := 'ATIVO';
      end;

      with RegistroI050.New do
      begin
         DT_ALT := StrToDate('01/01/2010');
         COD_NAT := '01';
         IND_CTA := 'S';
         NIVEL := '2';
         COD_CTA := '2328.2';
         COD_CTA_SUP := '2328A';
         CTA := 'ATIVO - ESTOQUES';
      end;

      with RegistroI050.New do
      begin
         DT_ALT := StrToDate('01/01/2010');
         COD_NAT := '01';
         IND_CTA := 'A';
         NIVEL := '3';
         COD_CTA := '2328.2.0001';
         COD_CTA_SUP := '2328.2';
         CTA := 'ESTOQUES - MATERIA-PRIMA';
         with RegistroI051.New do
         begin
            COD_ENT_REF := '10';
            COD_CCUS := '01';
            COD_CTA_REF := '1.01.03.01.00';
         end;
      end;

      with RegistroI075.New do
      begin
         COD_HIST := '232801';
         DESCR_HIST := 'COMPRA DE INSUMOS - NF. No.';
      end;

      with RegistroI100.New do
      begin
         DT_ALT  := StrToDate('01/01/2010');
         COD_CCUS := '01';
         CCUS := 'FABRICA';
      end;

      with RegistroI100.New do
      begin
         DT_ALT  := StrToDate('01/01/2010');
         COD_CCUS := '02';
         CCUS := 'COMERCIAL';
      end;                 

      with RegistroI150.New do
      begin
         DT_INI := StrToDate('01/01/2010');
         DT_FIN := StrToDate('31/12/2010');
         with RegistroI155.New do
         begin
            COD_CTA := '2328.2.0001';
            COD_CCUS := '01';
            VL_SLD_INI := 1000.00;
            IND_DC_INI := 'D';
            VL_DEB := 5000.55;
            VL_CRED := 5000.55;
            VL_SLD_FIN := 1000.00;
            IND_DC_FIN := 'D';
         end;
      end;

      with RegistroI200.New do
      begin
         NUM_LCTO := '101';
         DT_LCTO := Date;
         VL_LCTO := 5000.55;
         IND_LCTO := 'N';
         with RegistroI250.New do
         begin
            COD_CTA := '2328.2.0001';
            COD_CCUS := '01';
            VL_DC := 5000.55;
            IND_DC := 'C';
            NUM_ARQ := '111';
            COD_HIST_PAD := '232801';
            HIST := 'COMPRA DE INSUMOS - NF. No. 1001';
            COD_PART := '';
         end;
         with RegistroI250.New do
         begin
            COD_CTA := '2328.2.0001';
            COD_CCUS := '01';
            VL_DC := 5000.55;
            IND_DC := 'D';
            NUM_ARQ := '111';
            COD_HIST_PAD := '232801';
            HIST := 'COMPRA DE INSUMOS - NF. No. 1001';
            COD_PART := '';
         end;
      end;

      with RegistroI350.New do
      begin
         DT_RES := StrToDate('31/12/2010');
         with RegistroI355.New do
         begin
            COD_CTA := '2328.2.0001';
            COD_CCUS := '';
            VL_CTA := 1000.00;
            IND_DC := 'D';
         end;
      end;

   end;
end;

procedure TFrmSPEDFCont.btnB_JClick(Sender: TObject);
begin
   btnB_J.Enabled := false;

   ACBrSPEDFCont1.Bloco_J.LimpaRegistros;

   //Bloco J - Demonstrações contábeis
   with ACBrSPEDFCont1.Bloco_J do
   begin
      RegistroJ001.IND_DAD := 0;

      with RegistroJ930.New do
      begin
         IDENT_NOM := 'Representante Contador';
         IDENT_CPF := '57287228815';
         IDENT_QUALIF := 'Contador';
         COD_ASSIN := '900';
         IND_CRC := '1SP10734108'
      end;

      with RegistroJ930.New do
      begin
         IDENT_NOM := 'Representante Legal da Empresa XYZ';
         IDENT_CPF := '00908442700';
         IDENT_QUALIF := 'Diretor';
         COD_ASSIN := '203';
         IND_CRC := ''
      end;
   end;
end;

procedure TFrmSPEDFCont.btnErrorClick(Sender: TObject);
begin
   with ACBrSPEDFCont1 do
   begin
      DT_INI := StrToDate('01/01/2010');
      DT_FIN := StrToDate('31/12/2010');
   end;

   // Limpa a lista de erros.
   memoError.Lines.Clear;

   // Informa o pata onde será salvo o arquivo TXT.
   ACBrSPEDFCont1.Path := '.\';

   // Método que gera o arquivo TXT.
   ACBrSPEDFCont1.SaveFileTXT(edtFile.Text) ;

   // Habilita os botões
   btnB_0.Enabled := true;
   btnB_I.Enabled := true;
   btnB_J.Enabled := true;
   btnB_M.Enabled := true;
end;

procedure TFrmSPEDFCont.btnTXTClick(Sender: TObject);
begin
   btnTXT.Enabled := False ;

   with ACBrSPEDFCont1 do
   begin
      DT_INI := StrToDate('01/01/2010');
      DT_FIN := StrToDate('31/12/2010');
   end;

   // Limpa a lista de erros.
   memoError.Lines.Clear;

   // Informa o pata onde será salvo o arquivo TXT.
   ACBrSPEDFCont1.Path := '.\';

   // Método que gera o arquivo TXT.
   ACBrSPEDFCont1.SaveFileTXT(edtFile.Text) ;

   // Habilita os botões
   btnB_0.Enabled := true;
   btnB_I.Enabled := true;
   btnB_J.Enabled := true;
   btnB_M.Enabled := true;
   btnTXT.Enabled := True ;
end;

procedure TFrmSPEDFCont.btnB_MClick(Sender: TObject);
begin
   btnB_M.Enabled := false;

   ACBrSPEDFCont1.Bloco_M.LimpaRegistros;

   //Bloco M - Informações Fiscais
   with ACBrSPEDFCont1.Bloco_M do
   begin
      RegistroM001.IND_DAD := 0;

      // Bloco 0 - Identificação e referências
      with RegistroM020 do
      begin
         QUALI_PJ         := '10';
         TIPO_ESCRIT      := '0';
         NRO_REC_ANTERIOR := '';
      end;

      with RegistroM030.New do
      begin
         IND_PER     := 'A00';
         VL_LUC_LIQ  := 1000.00;
         IND_LUC_LIQ := 'C';
      end;

   end;
end;

procedure TFrmSPEDFCont.ACBrSPEDFCont1Error(const MsnError: String);
begin
   memoError.Lines.Add(MsnError);
end;

end.
