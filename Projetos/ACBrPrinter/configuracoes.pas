unit configuracoes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Spin, Buttons, ExtCtrls, types;

type

  { TfrConfiguracoes }

  TfrConfiguracoes = class(TForm)
    bbAtivar: TBitBtn;
    bImpressora: TButton;
    btnGravar: TBitBtn;
    btnCancelar: TBitBtn;
    cbControlePorta: TCheckBox;
    cbHRI: TCheckBox;
    cbIgnorarTags: TCheckBox;
    cbPreview: TCheckBox;
    cbTraduzirTags: TCheckBox;
    cbxExibeResumo: TCheckBox;
    cbxExibirEAN: TCheckBox;
    cbxExpandirLogo: TCheckBox;
    cbxFormCont: TCheckBox;
    cbxImpDescPorc: TCheckBox;
    cbxImpressora: TComboBox;
    cbxImpressoraNFCe: TComboBox;
    cbxImprimirDescAcresItemESCPOS: TCheckBox;
    cbxImprimirItem1LinhaESCPOS: TCheckBox;
    cbxImprimirTributos: TCheckBox;
    cbxImpValLiq: TCheckBox;
    cbxModelo: TComboBox;
    cbxMostrarPreview: TCheckBox;
    cbxMostraStatus: TCheckBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboBox;
    cbxImpressoraConf: TComboBox;
    edPosPrinterLog: TEdit;
    edtEspBorda: TEdit;
    edtFonteCampos: TEdit;
    edtFonteRazao: TEdit;
    edtLargCodProd: TEdit;
    edtMargemDir: TEdit;
    edtMargemEsq: TEdit;
    edtMargemInf: TEdit;
    edtMargemSup: TEdit;
    edtNumCopia: TEdit;
    gbCodBarras: TGroupBox;
    gbConfiguracao: TGroupBox;
    gbDANFeESCPOS: TGroupBox;
    gbGeral: TGroupBox;
    gbLogotipo: TGroupBox;
    gbQRCode: TGroupBox;
    gbxMargem: TGroupBox;
    GroupBox7: TGroupBox;
    Label1: TLabel;
    Label136: TLabel;
    Label139: TLabel;
    Label140: TLabel;
    Label141: TLabel;
    Label142: TLabel;
    Label143: TLabel;
    Label144: TLabel;
    Label145: TLabel;
    Label146: TLabel;
    Label147: TLabel;
    Label148: TLabel;
    Label149: TLabel;
    Label166: TLabel;
    Label174: TLabel;
    Label175: TLabel;
    Label176: TLabel;
    Label177: TLabel;
    Label178: TLabel;
    lbAltura: TLabel;
    lbBuffer: TLabel;
    lbColunas: TLabel;
    lbEspacosLinhas: TLabel;
    lbLargura: TLabel;
    lbLinhasPular: TLabel;
    lbLogoFatorX: TLabel;
    lbLogoFatorY: TLabel;
    lbLogoKC1: TLabel;
    lbLogoKC2: TLabel;
    lbModelo: TLabel;
    lbPorPrinterLog: TLabel;
    lbPorta: TLabel;
    lbQRCodeErrorLevel: TLabel;
    lbQRCodeLargMod: TLabel;
    lbQRCodeTipo: TLabel;
    lImpressora: TLabel;
    PageControl1: TPageControl;
    pgNFeNFCe: TPageControl;
    pnImpressora: TPanel;
    rgCasasDecimaisQtd: TRadioGroup;
    rgLocalCanhoto: TRadioGroup;
    rgModeloDANFeNFCE: TRadioGroup;
    rgModoImpressaoEvento: TRadioGroup;
    rgTipoDanfe: TRadioGroup;
    rgTipoFonte: TRadioGroup;
    sbPosPrinterLog: TSpeedButton;
    sbSerial: TSpeedButton;
    seBuffer: TSpinEdit;
    seCodBarrasAltura: TSpinEdit;
    seCodBarrasLargura: TSpinEdit;
    seColunas: TSpinEdit;
    seEspacosLinhas: TSpinEdit;
    seLargura: TSpinEdit;
    seLinhasPular: TSpinEdit;
    seLogoFatorX: TSpinEdit;
    seLogoFatorY: TSpinEdit;
    seLogoKC1: TSpinEdit;
    seLogoKC2: TSpinEdit;
    seMargemDireita: TSpinEdit;
    seMargemEsquerda: TSpinEdit;
    seMargemFundo: TSpinEdit;
    seMargemTopo: TSpinEdit;
    seQRCodeErrorLevel: TSpinEdit;
    seQRCodeLargMod: TSpinEdit;
    seQRCodeTipo: TSpinEdit;
    spedtDecimaisVUnit: TSpinEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    tsNFe: TTabSheet;
    tsNFCe: TTabSheet;
    tsDANFE: TTabSheet;
    tsSAT: TTabSheet;
    tsPosPrinter: TTabSheet;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frConfiguracoes: TfrConfiguracoes;

implementation

{$R *.lfm}

end.

