{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit configuracoes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, Buttons, ExtCtrls, Typinfo, IniFiles, Printers, fileinfo,
  ACBrPosPrinter;

type

  { TfrConfiguracoes }

  TfrConfiguracoes = class(TForm)
    bbTest: TBitBtn;
    btSave: TBitBtn;
    btCancel: TBitBtn;
    cbHRI: TCheckBox;
    cbIgnorarTags: TCheckBox;
    cbPreview: TCheckBox;
    cbTraduzirTags: TCheckBox;
    cbxExibirEAN: TCheckBox;
    cbxQuebraLinhaDetalhe: TCheckBox;
    cbxExpandirLogo: TCheckBox;
    cbxImpressora: TComboBox;
    cbxImpressoraNFCe: TComboBox;
    cbxImpressoraSAT: TComboBox;
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
    edtEmailEmpresa: TEdit;
    edtEspBorda: TSpinEdit;
    edtFonteRazao: TSpinEdit;
    edtLargCodProd: TSpinEdit;
    edtLogoMarca: TEdit;
    edtMargemDir: TFloatSpinEdit;
    edtMargemEsq: TFloatSpinEdit;
    edtMargemInf: TFloatSpinEdit;
    edtMargemSup: TFloatSpinEdit;
    edtSiteEmpresa: TEdit;
    edtSoftwareHouse: TEdit;
    seMargemSuperior: TFloatSpinEdit;
    seMargemInferior: TFloatSpinEdit;
    seMargemEsquerda: TFloatSpinEdit;
    seMargemDireita: TFloatSpinEdit;
    gbCodBarras: TGroupBox;
    gbConfiguracao: TGroupBox;
    gbDANFeESCPOS: TGroupBox;
    gbGeral: TGroupBox;
    gbLogotipo: TGroupBox;
    gbQRCode: TGroupBox;
    gbxMargem: TGroupBox;
    GroupBox7: TGroupBox;
    Label1: TLabel;
    Label121: TLabel;
    Label136: TLabel;
    Label139: TLabel;
    Label140: TLabel;
    Label141: TLabel;
    Label142: TLabel;
    Label143: TLabel;
    Label145: TLabel;
    Label146: TLabel;
    Label147: TLabel;
    Label148: TLabel;
    Label149: TLabel;
    Label150: TLabel;
    Label151: TLabel;
    Label153: TLabel;
    Label154: TLabel;
    Label166: TLabel;
    Label167: TLabel;
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
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    pgNFeNFCe: TPageControl;
    pnImpressora: TPanel;
    rgLocalCanhoto: TRadioGroup;
    rgModeloDANFeNFCE: TRadioGroup;
    rgModeloExtratoSAT: TRadioGroup;
    rgModoImpressaoEvento: TRadioGroup;
    rgTipoDanfe: TRadioGroup;
    rgTipoFonte: TRadioGroup;
    sbLogoMarca: TSpeedButton;
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
    seQRCodeErrorLevel: TSpinEdit;
    seQRCodeLargMod: TSpinEdit;
    seQRCodeTipo: TSpinEdit;
    spedtDecimaisVUnit: TSpinEdit;
    sbAdd: TSpeedButton;
    sbDelete: TSpeedButton;
    spedtDecimaisQtd: TSpinEdit;
    spedtNumCopias: TSpinEdit;
    tsDadosEmpresa: TTabSheet;
    tsNFe: TTabSheet;
    tsNFCe: TTabSheet;
    tsDANFE: TTabSheet;
    tsSAT: TTabSheet;
    tsPosPrinter: TTabSheet;
    procedure bbTestClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure cbxImpressoraConfSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbLogoMarcaClick(Sender: TObject);
    procedure sbSerialClick(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
  private
    procedure FindPrinters;
    procedure LoadConfig(FileConfig : String);
    procedure LoadDefaultValues;
    procedure SaveConfig(FileConfig : String);
  public

  end;

var
  frConfiguracoes: TfrConfiguracoes;
  sVersaoACBrPrinter : string;

implementation

uses
  ConfiguraSerial, ACBrPrinter1, ACBrUtil.Base, ACBrUtil.FilesIO;

{$R *.lfm}

{ TfrConfiguracoes }

procedure TfrConfiguracoes.FormCreate(Sender: TObject);
var
  iImpressoraESCPOS: TACBrPosPrinterModelo;
  iPagCodigoESCPOS: TACBrPosPaginaCodigo;
  FileVerInfo: TFileVersionInfo;
begin
  cbxImpressora.Items.Clear;
  cbxImpressora.Items.Assign(Printer.Printers);
  cbxImpressoraNFCe.Items.Clear;
  cbxImpressoraNFCe.Items.Assign(Printer.Printers);
  cbxImpressoraSAT.Items.Clear;
  cbxImpressoraSAT.Items.Assign(Printer.Printers);

  {PosPrinter}
  cbxModelo.Items.Clear;
  for iImpressoraESCPOS := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
    cbxModelo.Items.Add(GetEnumName(TypeInfo(TACBrPosPrinterModelo), Integer(iImpressoraESCPOS)));

  cbxPagCodigo.Items.Clear;
  for iPagCodigoESCPOS := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
    cbxPagCodigo.Items.Add(GetEnumName(TypeInfo(TACBrPosPaginaCodigo), Integer(iPagCodigoESCPOS)));

  cbxPorta.Items.Clear;
  DataModule1.ACBrPosPrinter1.Device.AcharPortasSeriais(cbxPorta.Items);
  cbxPorta.Items.Add('LPT1');
  cbxPorta.Items.Add('LPT2');
  cbxPorta.Items.Add('/dev/ttyS0');
  cbxPorta.Items.Add('/dev/ttyS1');
  cbxPorta.Items.Add('/dev/ttyUSB0');
  cbxPorta.Items.Add('/dev/ttyUSB1');
  cbxPorta.Items.Add('\\localhost\Epson');
  cbxPorta.Items.Add('c:\temp\ecf.txt');
  cbxPorta.Items.Add('/temp/ecf.txt');

  FindPrinters;

  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=paramstr(0);
    FileVerInfo.ReadFileInfo;
    sVersaoACBrPrinter := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

procedure TfrConfiguracoes.FormShow(Sender: TObject);
begin
  self.Caption := ' ACBrPrinter ' + sVersaoACBrPrinter + ' ';
end;

procedure TfrConfiguracoes.sbLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.png';
  OpenDialog1.Filter :=
    'Arquivos PNG (*.png)|Arquivos JPG (*.jpg)|Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtLogoMarca.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrConfiguracoes.cbxImpressoraConfSelect(Sender: TObject);
begin
  LoadConfig(cbxImpressoraConf.Text);
end;

procedure TfrConfiguracoes.bbTestClick(Sender: TObject);
var
  SL : TStringList;
begin
  LoadConfig(cbxImpressoraConf.Text);

  DataModule1.ReadConfigPosPrinter(cbxImpressoraConf.Text);

  SL := TStringList.Create;
  try
    SL.Add('</zera>');
    SL.Add('</linha_dupla>');
    SL.Add('FONTE NORMAL: '+IntToStr(DataModule1.ACBrPosPrinter1.ColunasFonteNormal)+' Colunas');
    SL.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', DataModule1.ACBrPosPrinter1.ColunasFonteNormal));
    SL.Add('<e>EXPANDIDO: '+IntToStr(DataModule1.ACBrPosPrinter1.ColunasFonteExpandida)+' Colunas');
    SL.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', DataModule1.ACBrPosPrinter1.ColunasFonteExpandida));
    SL.Add('</e><c>CONDENSADO: '+IntToStr(DataModule1.ACBrPosPrinter1.ColunasFonteCondensada)+' Colunas');
    SL.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', DataModule1.ACBrPosPrinter1.ColunasFonteCondensada));
    SL.Add('</c><n>FONTE NEGRITO</N>');
    SL.Add('<in>FONTE INVERTIDA</in>');
    SL.Add('<S>FONTE SUBLINHADA</s>');
    SL.Add('<i>FONTE ITALICO</i>');
    SL.Add('FONTE NORMAL');
    SL.Add('</linha_simples>');
    SL.Add('<n>LIGA NEGRITO');
    SL.Add('<i>LIGA ITALICO');
    SL.Add('<S>LIGA SUBLINHADA');
    SL.Add('<c>LIGA CONDENSADA');
    SL.Add('<e>LIGA EXPANDIDA');
    SL.Add('</fn>FONTE NORMAL');
    SL.Add('</linha_simples>');
    SL.Add('<e><n>NEGRITO E EXPANDIDA</n></e>');
    SL.Add('</fn>FONTE NORMAL');
    SL.Add('<in><c>INVERTIDA E CONDENSADA</c></in>');
    SL.Add('</fn>FONTE NORMAL');
    SL.Add('</linha_simples>');
    SL.Add('</FB>FONTE TIPO B');
    SL.Add('<n>FONTE NEGRITO</N>');
    SL.Add('<e>FONTE EXPANDIDA</e>');
    SL.Add('<in>FONTE INVERTIDA</in>');
    SL.Add('<S>FONTE SUBLINHADA</s>');
    SL.Add('<i>FONTE ITALICO</i>');
    SL.Add('</FA>FONTE TIPO A');
    SL.Add('</FN>FONTE NORMAL');
    SL.Add('</corte_total>');

    DataModule1.ACBrPosPrinter1.Imprimir(SL.Text);
  finally
    SL.Free;
  end;



end;

procedure TfrConfiguracoes.btCancelClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrConfiguracoes.btSaveClick(Sender: TObject);
begin
  SaveConfig(cbxImpressoraConf.Text);
end;

procedure TfrConfiguracoes.sbSerialClick(Sender: TObject);
var
  frConfiguraSerial: TfrConfiguraSerial;
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(Self);

  try
    frConfiguraSerial.Device.Porta        := cbxPorta.Text;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text;
    frConfiguraSerial.Device.ParamsString := DataModule1.ACBrPosPrinter1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOK then
    begin
      cbxPorta.Text := frConfiguraSerial.Device.Porta;
      DataModule1.ACBrPosPrinter1.Device.ParamsString  := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil(frConfiguraSerial);
  end;
end;

procedure TfrConfiguracoes.sbAddClick(Sender: TObject);
begin
  LoadDefaultValues;
end;

procedure TfrConfiguracoes.sbDeleteClick(Sender: TObject);
begin
  if cbxImpressoraConf.Items.Count <= 0 then
   begin
     MessageDlg('Não existe configuração a ser excluída.',mtError,[mbok],0);
     exit;
   end;

  if MessageDlg('Deseja realmente excluir esta configuração de impressora?',
                 mtInformation,[mbYes,mbNo],0) = mrYes then
  begin
    if not DeleteFile(PathWithDelim(ExtractFilePath(Application.ExeName))+cbxImpressoraConf.Text) then
     MessageDlg('Não foi possível excluir o arquivo '+PathWithDelim(ExtractFilePath(Application.ExeName))+cbxImpressoraConf.Text, mtError, [mbOK], 0);

    FindPrinters;
  end;
end;

procedure TfrConfiguracoes.FindPrinters;
var
  NomeArqConf : String;
  RetFind: Integer;
  SearchRec: TSearchRec;
begin
  cbxImpressoraConf.Clear;

  NomeArqConf := PathWithDelim(ExtractFilePath(Application.ExeName)) + '*.ini';
  RetFind := SysUtils.FindFirst(NomeArqConf, faAnyFile, SearchRec);

  while RetFind = 0 do
  begin
    cbxImpressoraConf.Items.Add(SearchRec.Name);
    RetFind := SysUtils.FindNext(SearchRec);
  end;

  cbxImpressoraConf.ItemIndex := 0;

  LoadConfig(cbxImpressoraConf.Text);
end;

procedure TfrConfiguracoes.LoadConfig(FileConfig: String);
var
  Ini: TIniFile;
begin
  FileConfig := PathWithDelim(ExtractFilePath(Application.ExeName))+FileConfig;

  if not EstaVazio(FileConfig) then
  begin
    Ini := TIniFile.Create(FileConfig);
    try
      cbxModelo.ItemIndex     := INI.ReadInteger('PosPrinter', 'Modelo', Integer(DataModule1.ACBrPosPrinter1.Modelo));
      cbxPorta.Text           := INI.ReadString('PosPrinter', 'Porta', DataModule1.ACBrPosPrinter1.Porta);
      seColunas.Value         := INI.ReadInteger('PosPrinter', 'Colunas', DataModule1.ACBrPosPrinter1.ColunasFonteNormal);
      seEspacosLinhas.Value   := INI.ReadInteger('PosPrinter', 'EspacoEntreLinhas', DataModule1.ACBrPosPrinter1.EspacoEntreLinhas);
      seBuffer.Value          := INI.ReadInteger('PosPrinter', 'LinhasBuffer', DataModule1.ACBrPosPrinter1.LinhasBuffer);
      seLinhasPular.Value     := INI.ReadInteger('PosPrinter', 'LinhasPular', 7);
      cbxPagCodigo.ItemIndex  := INI.ReadInteger('PosPrinter', 'PaginaDeCodigo', Integer(DataModule1.ACBrPosPrinter1.PaginaDeCodigo));
      cbTraduzirTags.Checked  := INI.ReadBool('PosPrinter', 'TraduzirTags', DataModule1.ACBrPosPrinter1.TraduzirTags);
      cbIgnorarTags.Checked   := INI.ReadBool('PosPrinter', 'IgnorarTags', DataModule1.ACBrPosPrinter1.IgnorarTags);
      edPosPrinterLog.Text    := INI.ReadString('PosPrinter', 'ArqLog', DataModule1.ACBrPosPrinter1.ArqLOG);

      seCodBarrasLargura.Value := INI.ReadInteger('Barras', 'Largura', DataModule1.ACBrPosPrinter1.ConfigBarras.LarguraLinha);
      seCodBarrasAltura.Value  := INI.ReadInteger('Barras', 'Altura', DataModule1.ACBrPosPrinter1.ConfigBarras.Altura);
      cbHRI.Checked            := INI.ReadBool('Barras', 'HRI', DataModule1.ACBrPosPrinter1.ConfigBarras.MostrarCodigo);

      seQRCodeTipo.Value       := INI.ReadInteger('QRCode', 'Tipo', DataModule1.ACBrPosPrinter1.ConfigQRCode.Tipo);
      seQRCodeLargMod.Value    := INI.ReadInteger('QRCode', 'LarguraModulo', DataModule1.ACBrPosPrinter1.ConfigQRCode.LarguraModulo);
      seQRCodeErrorLevel.Value := INI.ReadInteger('QRCode', 'ErrorLevel', DataModule1.ACBrPosPrinter1.ConfigQRCode.ErrorLevel);

      seLogoKC1.Value    := INI.ReadInteger('Logo', 'KC1', DataModule1.ACBrPosPrinter1.ConfigLogo.KeyCode1);
      seLogoKC2.Value    := INI.ReadInteger('Logo', 'KC2', DataModule1.ACBrPosPrinter1.ConfigLogo.KeyCode2);
      seLogoFatorX.Value := INI.ReadInteger('Logo', 'FatorX', DataModule1.ACBrPosPrinter1.ConfigLogo.FatorX);
      seLogoFatorY.Value := INI.ReadInteger('Logo', 'FatorY', DataModule1.ACBrPosPrinter1.ConfigLogo.FatorY);

      cbxImpressora.ItemIndex := cbxImpressora.Items.IndexOf(Ini.ReadString('NFe', 'Impressora', '0'));
      rgTipoDanfe.ItemIndex := Ini.ReadInteger('NFe', 'DANFE', 0);
      edtLogoMarca.Text := Ini.ReadString('DANFE', 'LogoMarca', '');
      edtSoftwareHouse.Text := Ini.ReadString('DANFE', 'SoftwareHouse', '');
      edtSiteEmpresa.Text := Ini.ReadString('DANFE', 'Site', '');
      edtEmailEmpresa.Text := Ini.ReadString('DANFE', 'Email', '');
      cbxMostrarPreview.Checked := Ini.ReadBool('DANFE', 'MostrarPreview', False);
      spedtNumCopias.Value := Ini.ReadInteger('DANFE', 'Copias', 1);
      edtLargCodProd.Value := Ini.ReadInteger('DANFE', 'LarguraCodigoProduto', 54);
      edtEspBorda.Value := Ini.ReadInteger('DANFE', 'EspessuraBorda', 1);
      edtFonteRazao.Value := Ini.ReadInteger('DANFE', 'FonteRazao', 12);
      edtMargemInf.Value := Ini.ReadFloat('DANFE', 'Margem', 0.8);
      edtMargemSup.Value := Ini.ReadFloat('DANFE', 'MargemSup', 0.8);
      edtMargemDir.Value := Ini.ReadFloat('DANFE', 'MargemDir', 0.51);
      edtMargemEsq.Value := Ini.ReadFloat('DANFE', 'MargemEsq', 0.6);
      spedtDecimaisQtd.Value := Ini.ReadInteger('DANFE', 'DecimaisQTD', 2);
      spedtDecimaisVUnit.Value := Ini.ReadInteger('DANFE', 'DecimaisValor', 2);
      cbxImprimirTributos.Checked := Ini.ReadBool('DANFE', 'ImprimirTributosItem', False);
      cbxImpValLiq.Checked := Ini.ReadBool('DANFE', 'ImprimirValLiq', False);
      cbxMostraStatus.Checked := Ini.ReadBool('DANFE', 'MostrarStatus', False);
      cbxExpandirLogo.Checked := Ini.ReadBool('DANFE', 'ExpandirLogo', False);
      rgTipoFonte.ItemIndex := Ini.ReadInteger('DANFE', 'Fonte', 0);
      rgLocalCanhoto.ItemIndex := Ini.ReadInteger('DANFE', 'LocalCanhoto', 0);
      cbxQuebraLinhaDetalhe.Checked := Ini.ReadBool('DANFE', 'QuebraLinhaEmDetalhe', False);

      rgModeloDANFeNFCE.ItemIndex := Ini.ReadInteger('NFCe', 'Modelo', 0);
      rgModoImpressaoEvento.ItemIndex := Ini.ReadInteger('NFCe', 'ModoImpressaoEvento', 0);
      cbxImprimirItem1LinhaESCPOS.Checked := Ini.ReadBool('NFCe', 'ImprimirItem1Linha', True);
      cbxImprimirDescAcresItemESCPOS.Checked := Ini.ReadBool('NFCe', 'ImprimirDescAcresItem', True);
      cbxImpressoraNFCe.ItemIndex := cbxImpressoraNFCe.Items.IndexOf(Ini.ReadString('NFCe', 'ImpressoraPadrao', '0'));

      if INI.ReadBool('SATFortes','UsarFortes', False) then
        rgModeloExtratoSAT.ItemIndex := 0
      else
        rgModeloExtratoSAT.ItemIndex := 1;

      seLargura.Value        := INI.ReadInteger('SATFortes','Largura',DataModule1.ACBrSATExtratoFortes1.LarguraBobina);
      seMargemSuperior.Value := INI.ReadFloat('SATFortes','MargemTopo',DataModule1.ACBrSATExtratoFortes1.MargemSuperior);
      seMargemInferior.Value := INI.ReadFloat('SATFortes','MargemFundo',DataModule1.ACBrSATExtratoFortes1.MargemInferior);
      seMargemEsquerda.Value := INI.ReadFloat('SATFortes','MargemEsquerda',DataModule1.ACBrSATExtratoFortes1.MargemEsquerda);
      seMargemDireita.Value  := INI.ReadFloat('Fortes','MargemDireita',DataModule1.ACBrSATExtratoFortes1.MargemDireita);
      cbPreview.Checked      := INI.ReadBool('SATFortes','Preview',False);

      cbxImpressoraSAT.ItemIndex := cbxImpressoraNFCe.Items.IndexOf(Ini.ReadString('SATPrinter','Name', '0'));
    finally
      Ini.Free;
    end;
  end;

end;

procedure TfrConfiguracoes.LoadDefaultValues;
begin
  cbxModelo.Text          := '';
  cbxPorta.Text           := '';
  seColunas.Value         := 48;
  seEspacosLinhas.Value   := 0;
  seBuffer.Value          := 0;
  seLinhasPular.Value     := 21;
  cbxPagCodigo.ItemIndex  := cbxPagCodigo.Items.IndexOf('pc850');
  cbTraduzirTags.Checked  := True;
  cbIgnorarTags.Checked   := False;
  edPosPrinterLog.Text    := '';

  seCodBarrasLargura.Value := DataModule1.ACBrPosPrinter1.ConfigBarras.LarguraLinha;
  seCodBarrasAltura.Value  := DataModule1.ACBrPosPrinter1.ConfigBarras.Altura;
  cbHRI.Checked            := DataModule1.ACBrPosPrinter1.ConfigBarras.MostrarCodigo;

  seQRCodeTipo.Value       := DataModule1.ACBrPosPrinter1.ConfigQRCode.Tipo;
  seQRCodeLargMod.Value    := DataModule1.ACBrPosPrinter1.ConfigQRCode.LarguraModulo;
  seQRCodeErrorLevel.Value := DataModule1.ACBrPosPrinter1.ConfigQRCode.ErrorLevel;

  seLogoKC1.Value    := DataModule1.ACBrPosPrinter1.ConfigLogo.KeyCode1;
  seLogoKC2.Value    := DataModule1.ACBrPosPrinter1.ConfigLogo.KeyCode2;
  seLogoFatorX.Value := DataModule1.ACBrPosPrinter1.ConfigLogo.FatorX;
  seLogoFatorY.Value := DataModule1.ACBrPosPrinter1.ConfigLogo.FatorY;

  cbxImpressora.Text := '';
  rgTipoDanfe.ItemIndex := 0;
  edtLogoMarca.Text := '';
  edtSoftwareHouse.Text := '';
  edtSiteEmpresa.Text := '';
  edtEmailEmpresa.Text := '';
  cbxMostrarPreview.Checked := False;
  spedtNumCopias.Value := 1;
  edtLargCodProd.Value := 54;
  edtEspBorda.Value := 1;
  edtFonteRazao.Value := 12;
  edtMargemInf.Value :=  0.8;
  edtMargemSup.Value :=  0.8;
  edtMargemDir.Value := 0.51;
  edtMargemEsq.Value := 0.6;
  spedtDecimaisQtd.Value := 2;
  spedtDecimaisVUnit.Value := 2;
  cbxImprimirTributos.Checked := False;
  cbxImpValLiq.Checked := False;
  cbxMostraStatus.Checked := False;
  cbxExpandirLogo.Checked := False;
  rgTipoFonte.ItemIndex := 0;
  rgLocalCanhoto.ItemIndex := 0;

  rgModeloDANFeNFCE.ItemIndex := 1;
  rgModoImpressaoEvento.ItemIndex := 1;
  cbxImprimirItem1LinhaESCPOS.Checked := True;
  cbxImprimirDescAcresItemESCPOS.Checked := True;
  cbxImpressoraNFCe.Text := '';

  rgModeloExtratoSAT.ItemIndex := 1;

  seLargura.Value        := DataModule1.ACBrSATExtratoFortes1.LarguraBobina;
  seMargemSuperior.Value := DataModule1.ACBrSATExtratoFortes1.MargemSuperior;
  seMargemInferior.Value := DataModule1.ACBrSATExtratoFortes1.MargemInferior;
  seMargemEsquerda.Value := DataModule1.ACBrSATExtratoFortes1.MargemEsquerda;
  seMargemDireita.Value  := DataModule1.ACBrSATExtratoFortes1.MargemDireita;
  cbPreview.Checked      := False;

  cbxImpressoraSAT.Text := '';
end;

procedure TfrConfiguracoes.SaveConfig(FileConfig: String);
var
  Ini: TIniFile;
begin
  FileConfig := PathWithDelim(ExtractFilePath(Application.ExeName))+FileConfig+'.ini';

  Ini := TIniFile.Create(FileConfig);
  try
    Ini.WriteInteger('PosPrinter', 'Modelo', cbxModelo.ItemIndex );
    Ini.WriteString('PosPrinter', 'Porta', cbxPorta.Text);
    Ini.WriteInteger('PosPrinter', 'Colunas', seColunas.Value);
    Ini.WriteInteger('PosPrinter', 'EspacoEntreLinhas', seEspacosLinhas.Value);
    Ini.WriteInteger('PosPrinter', 'LinhasBuffer', seBuffer.Value);
    Ini.WriteInteger('PosPrinter', 'LinhasPular', seLinhasPular.Value);
    Ini.WriteInteger('PosPrinter', 'PaginaDeCodigo', cbxPagCodigo.ItemIndex);
    Ini.WriteBool('PosPrinter', 'TraduzirTags', cbTraduzirTags.Checked);
    Ini.WriteBool('PosPrinter', 'IgnorarTags', cbIgnorarTags.Checked);
    Ini.WriteString('PosPrinter', 'ArqLog', edPosPrinterLog.Text);

    Ini.WriteInteger('Barras', 'Largura', seCodBarrasLargura.Value);
    Ini.WriteInteger('Barras', 'Altura', seCodBarrasAltura.Value);
    Ini.WriteBool('Barras', 'HRI', cbHRI.Checked);

    Ini.WriteInteger('QRCode', 'Tipo', seQRCodeTipo.Value);
    Ini.WriteInteger('QRCode', 'LarguraModulo', seQRCodeLargMod.Value);
    Ini.WriteInteger('QRCode', 'ErrorLevel', seQRCodeErrorLevel.Value);

    Ini.WriteInteger('Logo', 'KC1', seLogoKC1.Value);
    Ini.WriteInteger('Logo', 'KC2', seLogoKC2.Value);
    Ini.WriteInteger('Logo', 'FatorX', seLogoFatorX.Value);
    Ini.WriteInteger('Logo', 'FatorY', seLogoFatorY.Value);

    Ini.WriteString('NFe', 'Impressora', cbxImpressora.Text);
    Ini.WriteInteger('NFe', 'DANFE', rgTipoDanfe.ItemIndex);
    Ini.WriteString('DANFE', 'LogoMarca', edtLogoMarca.Text);
    Ini.WriteString('DANFE', 'SoftwareHouse', edtSoftwareHouse.Text);
    Ini.WriteString('DANFE', 'Site', edtSiteEmpresa.Text);
    Ini.WriteString('DANFE', 'Email', edtEmailEmpresa.Text);
    Ini.WriteBool('DANFE', 'MostrarPreview', cbxMostrarPreview.Checked);
    Ini.WriteInteger('DANFE', 'Copias', spedtNumCopias.Value );
    Ini.WriteInteger('DANFE', 'LarguraCodigoProduto', edtLargCodProd.Value);
    Ini.WriteInteger('DANFE', 'EspessuraBorda', edtEspBorda.Value);
    Ini.WriteInteger('DANFE', 'FonteRazao', edtFonteRazao.Value);
    Ini.WriteFloat('DANFE', 'Margem', edtMargemInf.Value);
    Ini.WriteFloat('DANFE', 'MargemSup', edtMargemSup.Value);
    Ini.WriteFloat('DANFE', 'MargemDir', edtMargemDir.Value);
    Ini.WriteFloat('DANFE', 'MargemEsq', edtMargemEsq.Value);
    Ini.WriteInteger('DANFE', 'DecimaisQTD', spedtDecimaisQtd.Value);
    Ini.WriteInteger('DANFE', 'DecimaisValor', spedtDecimaisVUnit.Value);
    Ini.WriteBool('DANFE', 'ImprimirTributosItem', cbxImprimirTributos.Checked);
    Ini.WriteBool('DANFE', 'ImprimirValLiq', cbxImpValLiq.Checked);
    Ini.WriteBool('DANFE', 'MostrarStatus', cbxMostraStatus.Checked);
    Ini.WriteBool('DANFE', 'ExpandirLogo', cbxExpandirLogo.Checked );
    Ini.WriteInteger('DANFE', 'Fonte', rgTipoFonte.ItemIndex);
    Ini.WriteInteger('DANFE', 'LocalCanhoto', rgLocalCanhoto.ItemIndex);
    Ini.WriteBool('DANFE', 'QuebraLinhaEmDetalhe', cbxQuebraLinhaDetalhe.Checked);

    Ini.WriteInteger('NFCe', 'Modelo', rgModeloDANFeNFCE.ItemIndex);
    Ini.WriteInteger('NFCe', 'ModoImpressaoEvento', rgModoImpressaoEvento.ItemIndex);
    Ini.WriteBool('NFCe', 'ImprimirItem1Linha', cbxImprimirItem1LinhaESCPOS.Checked);
    Ini.WriteBool('NFCe', 'ImprimirDescAcresItem', cbxImprimirDescAcresItemESCPOS.Checked);
    Ini.WriteString('NFCe', 'ImpressoraPadrao', cbxImpressoraNFCe.Text);

    if rgModeloExtratoSAT.ItemIndex = 0 then
       Ini.WriteBool('SATFortes','UsarFortes', True)
    else
      Ini.WriteBool('SATFortes','UsarFortes', False);

    Ini.WriteInteger('SATFortes','Largura',seLargura.Value);
    Ini.WriteFloat('SATFortes','MargemTopo',seMargemSuperior.Value);
    Ini.WriteFloat('SATFortes','MargemFundo',seMargemInferior.Value );
    Ini.WriteFloat('SATFortes','MargemEsquerda',seMargemEsquerda.Value);
    Ini.WriteFloat('Fortes','MargemDireita',seMargemDireita.Value);
    Ini.WriteBool('SATFortes','Preview',cbPreview.Checked );

    Ini.WriteString('SATPrinter','Name', cbxImpressoraSAT.Text);

  finally
    Ini.Free;
  end;
end;

end.

