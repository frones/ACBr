unit Frm_SPEDContabil;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, ACBrEFDBlocos,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ACBrUtil, ACBrTXTClass,
  ACBrSpedContabil;

type

  { TFrmSPEDContabil }

  TFrmSPEDContabil = class(TForm)
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
    ACBrSPEDContabil1: TACBrSPEDContabil;
    procedure ACBrSPEDContabil1Error(const MsnError: string);
    procedure btnB_0Click(Sender: TObject);
    procedure btnB_IClick(Sender: TObject);
    procedure btnB_JClick(Sender: TObject);
    procedure btnErrorClick(Sender: TObject);
    procedure btnTXTClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSPEDContabil: TFrmSPEDContabil;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

procedure TFrmSPEDContabil.ACBrSPEDContabil1Error(const MsnError: string);
begin
   memoError.Lines.Add(MsnError);
end;

procedure TFrmSPEDContabil.btnB_0Click(Sender: TObject);
begin
   btnB_0.Enabled := false;

   ACBrSpedContabil1.Bloco_0.LimpaRegistros;
   with ACBrSpedContabil1.Bloco_0 do
   begin
      Registro0001.IND_DAD := 0; //bloco com dados informados = 0 | sem dados inf = 1

      // Bloco 0 - Identificação e referências
      with Registro0000 do
      begin
         DT_INI  := StrToDate('01/01/2006');  //DateTimePicker1.DateTime;
         DT_FIN  := StrToDate('31/01/2006'); //DateTimePicker2.DateTime;
         NOME    := 'ESTABELECIMENTO TESTE DIÁRIO GERAL';
         CNPJ    := '00910509000171';
         UF      := 'SP';
         IE      := '5350546531180';
         COD_MUN := '3550308';
         IM      := '111111';
         IND_SIT_ESP := '';
      end;

      with Registro0007.New do
      begin
         COD_ENT_REF := 'PR';
         COD_INSCR := '1100002511';
      end;

      with Registro0150.New do
      begin
         COD_PART := '5-OS2328-PFJ004';
         NOME := 'PFJ 004';
         COD_PAIS := '00105';
         CNPJ := '61064911000177';
         UF := 'SP';
         IE := '535054653118';
         COD_MUN := 3550308;
         IM := '1122';
         SUFRAMA := 'SUFRAMA';
         with Registro0180.New do
         begin
            COD_REL := '04';
            DT_INI_REL := StrToDate('15/02/2004');
         end;
      end;

      with Registro0150.New do
      begin
         COD_PART := '5-OS2328-PFJ005';
         NOME := 'PFJ 005';
         COD_PAIS := '00105';
         CNPJ := '';
         UF := 'SP';
         IE := '';
         COD_MUN := 3550308;
         IM := '1122';
         SUFRAMA := '';
         with Registro0180.New do
         begin
            COD_REL := '09';
            DT_INI_REL := StrToDate('15/02/2004');
         end;
      end;

   end;
end;

procedure TFrmSPEDContabil.btnB_IClick(Sender: TObject);
begin
   btnB_I.Enabled := false;

   ACBrSpedContabil1.Bloco_I.LimpaRegistros;

   // Bloco I - Lançamentos Contábeis
   with ACBrSpedContabil1.Bloco_I do
   begin
      RegistroI001.IND_DAD := 0;

      RegistroI010.IND_ESC := 'G';
      RegistroI010.COD_VER_LC := '1.00';

     with RegistroI030 do
      begin
         NUM_ORD := '100';
         NAT_LIVR := 'DIARIO GERAL';
         QTD_LIN := 372;
         NOME := 'ESTABELECIMENTO TESTE DIÁRIO GERAL';
         NIRE := '35300095618';
         CNPJ := '00910509000171';
         DT_ARQ := StrToDate('01/01/2000');
         DT_ARQ_CONV := StrToDate('20/06/2000');
         DESC_MUN := 'SAO PAULO'
      end;

      with RegistroI050.New do
      begin
         DT_ALT := StrToDate('01/01/2004');
         COD_NAT := '01';
         IND_CTA := 'S';
         NIVEL := '1';
         COD_CTA := '2328A';
         COD_CTA_SUP := '';
         CTA := 'ATIVO';
      end;

      with RegistroI050.New do
      begin
         DT_ALT := StrToDate('01/01/2004');
         COD_NAT := '01';
         IND_CTA := 'S';
         NIVEL := '2';
         COD_CTA := '2328.2';
         COD_CTA_SUP := '2328A';
         CTA := 'ATIVO - ESTOQUES';
      end;

      with RegistroI050.New do
      begin
         DT_ALT := StrToDate('01/01/2004');
         COD_NAT := '01';
         IND_CTA := 'A';
         NIVEL := '3';
         COD_CTA := '2328.2.0001';
         COD_CTA_SUP := '2328.2';
         CTA := 'ESTOQUES - MATERIA-PRIMA';
         with RegistroI051.New do
         begin
            COD_PLAN_REF := '10';
            COD_CCUS := '';
            COD_CTA_REF := '1.01.03.01.00';
         end;
      end;

      with RegistroI075.New do
      begin
         COD_HIST := '232801';
         DESCR_HIST := 'COMPRA DE INSUMOS - NF. No.';
      end;

      with RegistroI150.New do
      begin
         DT_INI := StrToDate('01/01/2006');
         DT_FIN := StrToDate('31/01/2006');
         with RegistroI155.New do
         begin
            COD_CTA := '2328.2.0001';
            COD_CCUS := '';
            VL_SLD_INI := 0.00;
            IND_DC_INI := 'D';
            VL_DEB := 5000.00;
            VL_CRED := 5000.00;
            VL_SLD_FIN := 0.00;
            IND_DC_FIN := 'D';
         end;
      end;

      with RegistroI200.New do
      begin
         NUM_LCTO := '101';
         DT_LCTO := Date;
         VL_LCTO := 5000.00;
         IND_LCTO := 'N';
         with RegistroI250.New do
         begin
            COD_CTA := '2328.2.0001';
            COD_CCUS := '';
            VL_DC := 5000.00;
            IND_DC := 'C';
            NUM_ARQ := '111';
            COD_HIST_PAD := '232801';
            HIST := 'COMPRA DE INSUMOS - NF. No. 1001';
            COD_PART := '5-OS2328-PFJ004';
         end;
         with RegistroI250.New do
         begin
            COD_CTA := '2328.2.0001';
            COD_CCUS := '';
            VL_DC := 5000.00;
            IND_DC := 'D';
            NUM_ARQ := '111';
            COD_HIST_PAD := '232801';
            HIST := 'COMPRA DE INSUMOS - NF. No. 1001';
            COD_PART := '5-OS2328-PFJ004';
         end;
      end;

   end;
end;

procedure TFrmSPEDContabil.btnB_JClick(Sender: TObject);
begin
   btnB_J.Enabled := false;

   ACBrSpedContabil1.Bloco_J.LimpaRegistros;

   //Bloco J - Demonstrações contábeis
   with ACBrSpedContabil1.Bloco_J do
   begin
      RegistroJ001.IND_DAD := 0;

      with RegistroJ005.New do
      begin
        DT_INI := StrToDate('01/01/2006');
        DT_FIN := StrToDate('31/01/2006');
        ID_DEM := 1;

        with RegistroJ100.New do
        begin
          COD_AGL   := '1';
          NIVEL_AGL := '1';
          IND_GRP_BAL := '1';
          VL_CTA := 1;

        end;
      end;

      with RegistroJ005.New do
      begin
        DT_INI := StrToDate('01/02/2006');
        DT_FIN := StrToDate('28/02/2006');
        ID_DEM := 2;

        with RegistroJ100.New do
        begin
          COD_AGL   := '2';
          NIVEL_AGL := '2';
          IND_GRP_BAL := '2';
          VL_CTA := 2;
        end;
      end;

      with RegistroJ900 do
      begin
         NUM_ORD := '100';
         NAT_LIVRO := 'DIÁRIO GERAL';
         NOME := 'ESTABELECIMENTO TESTE DIÁRIO GERAL';
         QTD_LIN := 119;
         DT_INI_ESCR := StrToDate('01/01/2006');
         DT_FIN_ESCR := StrToDate('31/01/2006');
      end;

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

procedure TFrmSPEDContabil.btnErrorClick(Sender: TObject);
begin
   with ACBrSpedContabil1 do
   begin
      DT_INI := StrToDate('01/07/2009');
      DT_FIN := StrToDate('01/07/2009');
   end;

   // Limpa a lista de erros.
   memoError.Lines.Clear;

   // Informa o path onde será salvo o arquivo TXT.
   ACBrSPEDContabil1.Path := '.\';

   // Informa o nome do arquivo TXT
   ACBrSPEDContabil1.Arquivo:=edtFile.Text;

   // Método que gera o arquivo TXT.
   ACBrSpedContabil1.SaveFileTXT() ;

   // Habilita os botões
   btnB_0.Enabled := true;
   btnB_I.Enabled := true;
   btnB_J.Enabled := true;
end;

procedure TFrmSPEDContabil.btnTXTClick(Sender: TObject);
begin
   btnTXT.Enabled := False ;

   with ACBrSPEDContabil1 do
   begin
      DT_INI := StrToDate('01/07/2009');
      DT_FIN := StrToDate('31/07/2009');
   end;

   // Limpa a lista de erros.
   memoError.Lines.Clear;
   memoTXT.Clear;

   // Informa o pata onde será salvo o arquivo TXT.
   ACBrSPEDContabil1.Path := '.\';

   ACBrSPEDContabil1.Arquivo:=edtFile.Text;

   // Método que gera o arquivo TXT.
   ACBrSPEDContabil1.SaveFileTXT() ;

   memoTXT.Lines.LoadFromFile(edtFile.Text);

   // Habilita os botões
   btnB_0.Enabled := true;
   btnB_I.Enabled := true;
   btnB_J.Enabled := true;
   btnTXT.Enabled := True ;
end;

end.
