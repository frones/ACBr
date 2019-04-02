unit UFormImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ACBrSpedFiscal, DB, DBClient, ACBrEFDImportar, Grids, DBGrids,
  ACBrEFDBloco_C, ACBrEFDBlocos, ACBrBase, ACBrEPCImportar;

type
  TFormImport = class(TForm)
    OpenDlg: TOpenDialog;
    CDS_C100: TClientDataSet;
    DS_C100: TDataSource;
    Button1: TButton;
    EdtFile: TEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Grid_C100: TDBGrid;
    Grid_C170: TDBGrid;
    CDS_C170: TClientDataSet;
    DS_C170: TDataSource;
    CDS_C100IND_OPER: TIntegerField;
    CDS_C100IND_EMIT: TIntegerField;
    CDS_C100COD_PART: TStringField;
    CDS_C100COD_MOD: TStringField;
    CDS_C100COD_SIT: TIntegerField;
    CDS_C100SER: TStringField;
    CDS_C100NUM_DOC: TStringField;
    CDS_C100CHV_NFE: TStringField;
    CDS_C100DT_DOC: TDateField;
    CDS_C100DT_E_S: TDateField;
    CDS_C100VL_DOC: TFloatField;
    CDS_C100IND_PGTO: TIntegerField;
    CDS_C100VL_DESC: TFloatField;
    CDS_C100VL_ABAT_NT: TFloatField;
    CDS_C100VL_MERC: TFloatField;
    CDS_C100IND_FRT: TIntegerField;
    CDS_C100VL_FRT: TFloatField;
    CDS_C100VL_SEG: TFloatField;
    CDS_C100VL_OUT_DA: TFloatField;
    CDS_C100VL_BC_ICMS: TFloatField;
    CDS_C100VL_ICMS: TFloatField;
    CDS_C100VL_BC_ICMS_ST: TFloatField;
    CDS_C100VL_ICMS_ST: TFloatField;
    CDS_C100VL_IPI: TFloatField;
    CDS_C100VL_PIS: TFloatField;
    CDS_C100VL_COFINS: TFloatField;
    CDS_C100VL_PIS_ST: TFloatField;
    CDS_C100VL_COFINS_ST: TFloatField;
    CDS_C170NUM_ITEM: TStringField;
    CDS_C170COD_ITEM: TStringField;
    CDS_C170DESCR_COMPL: TStringField;
    CDS_C170QTD: TFloatField;
    CDS_C170UNID: TStringField;
    CDS_C170VL_ITEM: TFloatField;
    CDS_C170VL_DESC: TFloatField;
    CDS_C170IND_MOV: TIntegerField;
    CDS_C170CST_ICMS: TStringField;
    CDS_C170CFOP: TStringField;
    CDS_C170COD_NAT: TStringField;
    CDS_C170VL_BC_ICMS: TFloatField;
    CDS_C170ALIQ_ICMS: TFloatField;
    CDS_C170VL_ICMS: TFloatField;
    CDS_C170VL_BC_ICMS_ST: TFloatField;
    CDS_C170ALIQ_ST: TFloatField;
    CDS_C170VL_ICMS_ST: TFloatField;
    CDS_C170IND_APUR: TIntegerField;
    CDS_C170CST_IPI: TStringField;
    CDS_C170COD_ENQ: TStringField;
    CDS_C170VL_BC_IPI: TFloatField;
    CDS_C170ALIQ_IPI: TFloatField;
    CDS_C170VL_IPI: TFloatField;
    CDS_C170CST_PIS: TStringField;
    CDS_C170VL_BC_PIS: TFloatField;
    CDS_C170ALIQ_PIS_PERC: TFloatField;
    CDS_C170QUANT_BC_PIS: TFloatField;
    CDS_C170ALIQ_PIS_R: TFloatField;
    CDS_C170VL_PIS: TFloatField;
    CDS_C170CST_COFINS: TStringField;
    CDS_C170VL_BC_COFINS: TFloatField;
    CDS_C170ALIQ_COFINS_PERC: TFloatField;
    CDS_C170QUANT_BC_COFINS: TFloatField;
    CDS_C170ALIQ_COFINS_R: TFloatField;
    CDS_C170VL_COFINS: TFloatField;
    CDS_C170COD_CTA: TStringField;
    CDS_C100ID: TIntegerField;
    CDS_C170PARENT: TIntegerField;
    Label2: TLabel;
    LblEmp: TLabel;
    SpedPCImp: TACBrSpedFiscalImportar;
    SPEDFiscal: TACBrSPEDFiscal;
    procedure Button1Click(Sender: TObject);
    procedure CDS_C100AfterScroll(DataSet: TDataSet);
  private
    { Private declarations }
    procedure Gen_C100;
    procedure Gen_C170(AParent: Integer; ARegC170: TRegistroC170List);
    procedure RestartDataSet;
  public
    { Public declarations }
  end;

var
  FormImport: TFormImport;

implementation

{$R *.dfm}

procedure TFormImport.Button1Click(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    EdtFile.Text := OpenDlg.FileName;
    SpedPCImp.Arquivo := EdtFile.Text;
    SpedPCImp.Importar;
    LblEmp.Caption := 'Empresa: ' + SPEDFiscal.Bloco_0.Registro0000.NOME;
    Gen_C100;
  end;
end;

procedure TFormImport.CDS_C100AfterScroll(DataSet: TDataSet);
begin
  if not CDS_C100ID.IsNull then
    CDS_C170.Filter := 'PARENT = ' + CDS_C100ID.AsString;
end;

procedure TFormImport.Gen_C100;
var
  I: Integer;
  J: Integer;
begin
  RestartDataSet;

  CDS_C100.DisableControls;
  CDS_C170.DisableControls;
  try
    with SPEDFiscal.Bloco_C.RegistroC001 do
//      for I := 0 to RegistroC010.Count - 1 do
  //      with RegistroC010.Items[I] do
          for J := 0 to RegistroC100.Count - 1 do
            with RegistroC100.Items[J] do
            begin
              CDS_C100.Append;
              CDS_C100IND_OPER.AsInteger    := Ord(IND_OPER);
              CDS_C100IND_EMIT.AsInteger    := Ord(IND_EMIT);
              CDS_C100COD_PART.AsString     := COD_PART;
              CDS_C100COD_MOD.AsString      := COD_MOD;
//              CDS_C100COD_SIT.AsString      :=  COD_SIT;
              CDS_C100SER.AsString          := SER;
              CDS_C100NUM_DOC.AsString      := NUM_DOC;
              CDS_C100CHV_NFE.AsString      := CHV_NFE;
              CDS_C100DT_DOC.AsDateTime     := DT_DOC;
              CDS_C100DT_E_S.AsDateTime     := DT_E_S;
              CDS_C100VL_DOC.AsFloat        := VL_DOC;
//              CDS_C100IND_PGTO.AsString     := IndPgtoToStr(IND_PGTO);
              CDS_C100VL_DESC.AsFloat       := VL_DESC;
              CDS_C100VL_ABAT_NT.AsFloat    := VL_ABAT_NT;
              CDS_C100VL_MERC.AsFloat       := VL_MERC;
//              CDS_C100IND_FRT.AsString      := IND_FRT;
              CDS_C100VL_FRT.AsFloat        := VL_FRT;
              CDS_C100VL_SEG.AsFloat        := VL_SEG;
              CDS_C100VL_OUT_DA.AsFloat     := VL_OUT_DA;
              CDS_C100VL_BC_ICMS.AsFloat    := VL_BC_ICMS;
              CDS_C100VL_ICMS.AsFloat       := VL_ICMS;
              CDS_C100VL_BC_ICMS_ST.AsFloat := VL_BC_ICMS_ST;
              CDS_C100VL_ICMS_ST.AsFloat    := VL_ICMS_ST;
              CDS_C100VL_IPI.AsFloat        := VL_IPI;
              CDS_C100VL_PIS.AsFloat        := VL_PIS;
              CDS_C100VL_COFINS.AsFloat     := VL_COFINS;
              CDS_C100VL_PIS_ST.AsFloat     := VL_PIS_ST;
              CDS_C100VL_COFINS_ST.AsFloat  := VL_COFINS_ST;
              CDS_C100ID.AsInteger          := J;
              CDS_C100.Post;
              Gen_C170(J, RegistroC170);
            end;
  finally
    CDS_C100.EnableControls;
    CDS_C170.EnableControls;
  end;
end;

procedure TFormImport.Gen_C170(AParent: Integer; ARegC170: TRegistroC170List);
var
  I: Integer;
begin
  for I := 0 to ARegC170.Count - 1 do
    with ARegC170.Items[I] do
    begin
      CDS_C170.Append;
      CDS_C170NUM_ITEM.AsString     := NUM_ITEM;
      CDS_C170COD_ITEM.AsString     := COD_ITEM;
      CDS_C170DESCR_COMPL.AsString  := DESCR_COMPL;
      CDS_C170QTD.AsFloat           := QTD;
      CDS_C170UNID.AsString         := UNID;
      CDS_C170VL_ITEM.AsFloat       := VL_ITEM;
      CDS_C170VL_DESC.AsFloat       := VL_DESC;
      CDS_C170IND_MOV.AsString      := IndMovFisicaToStr(IND_MOV);
      CDS_C170CST_ICMS.AsString     :=  CST_ICMS;
      CDS_C170CFOP.AsString         := CFOP;
      CDS_C170COD_NAT.AsString      := COD_NAT;
      CDS_C170VL_BC_ICMS.AsFloat    := VL_BC_ICMS;
      CDS_C170ALIQ_ICMS.AsFloat     := ALIQ_ICMS;
      CDS_C170VL_ICMS.AsFloat       := VL_ICMS;
      CDS_C170VL_BC_ICMS_ST.AsFloat := VL_BC_ICMS_ST;
      CDS_C170ALIQ_ST.AsFloat       := ALIQ_ST;
      CDS_C170VL_ICMS_ST.AsFloat    := VL_ICMS_ST;
      CDS_C170IND_APUR.AsInteger    := Ord(IND_APUR);
      CDS_C170CST_IPI.AsString      := CST_IPI;
      CDS_C170COD_ENQ.AsString      := COD_ENQ;
      CDS_C170VL_BC_IPI.AsFloat     := VL_BC_IPI;
      CDS_C170ALIQ_IPI.AsFloat      := ALIQ_IPI;
      CDS_C170VL_IPI.AsFloat        := VL_IPI;
      CDS_C170CST_PIS.AsString      := CST_PIS;
      CDS_C170VL_BC_PIS.AsVariant   := VL_BC_PIS;
      CDS_C170ALIQ_PIS_PERC.AsVariant := ALIQ_PIS_PERC;
      CDS_C170QUANT_BC_PIS.AsVariant  := QUANT_BC_PIS;
      CDS_C170ALIQ_PIS_R.AsVariant    := ALIQ_PIS_R;
      CDS_C170VL_PIS.AsVariant        := VL_PIS;
      CDS_C170CST_COFINS.AsString        := CST_COFINS;
      CDS_C170VL_BC_COFINS.AsVariant     := VL_BC_COFINS;
      CDS_C170ALIQ_COFINS_PERC.AsVariant := ALIQ_COFINS_PERC;
      CDS_C170QUANT_BC_COFINS.AsVariant  := QUANT_BC_COFINS;
      CDS_C170ALIQ_COFINS_R.AsVariant    := ALIQ_COFINS_R;
      CDS_C170VL_COFINS.AsVariant        := VL_COFINS;
      CDS_C170COD_CTA.AsString         := COD_CTA;
      CDS_C170PARENT.AsInteger         := AParent;
      CDS_C170.Post;
    end;
end;

procedure TFormImport.RestartDataSet;
begin
  if CDS_C100.Active then
    CDS_C100.EmptyDataSet;
  CDS_C100.Close;
  CDS_C100.CreateDataSet;

  if CDS_C170.Active then
    CDS_C170.EmptyDataSet;
  CDS_C170.Close;
  CDS_C170.CreateDataSet;
end;

end.
