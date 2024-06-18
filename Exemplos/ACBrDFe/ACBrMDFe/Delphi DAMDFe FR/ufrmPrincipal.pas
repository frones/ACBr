  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  {                                                                              }
  { Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
  {																			   }
  { Colaboradores nesse arquivo: Juliomar Marchetti                              }
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

unit ufrmPrincipal;

{$I ACBr_jedi.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  ACBrUtil,
  ACBrBase,
  ACBrDFe,
  frxClass,
  ACBrDFeReport,
  ACBrMDFeDAMDFeClass,
  ACBrMDFeDAMDFEFR,
  ACBrMDFe, frxExportBaseDialog, frxExportPDF, frxPDFViewer;

type
  TfrmPrincipal = class(TForm)
    imgLogo: TImage;
    pnlbotoes: TPanel;
    btnImprimir: TButton;
    btncarregar: TButton;
    btnCarregarEvento: TButton;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    frxReport1: TfrxReport;
    PageControl1: TPageControl;
    TabArquivos: TTabSheet;
    lstbxFR3: TListBox;
    TabCustomizacao: TTabSheet;
    RbCanhoto: TRadioGroup;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditMargemEsquerda: TEdit;
    EditMargemSuperior: TEdit;
    EditMargemDireita: TEdit;
    EditMargemInferior: TEdit;
    Decimais: TTabSheet;
    RgTipodedecimais: TRadioGroup;
    PageControl2: TPageControl;
    TabtdetInteger: TTabSheet;
    TabtdetMascara: TTabSheet;
    cbtdetInteger_qtd: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    cbtdetInteger_Vrl: TComboBox;
    Label7: TLabel;
    cbtdetMascara_qtd: TComboBox;
    Label8: TLabel;
    cbtdetMascara_Vrl: TComboBox;
    rbTarjaCancelada: TCheckBox;
    Label9: TLabel;
    CBImprimirUndQtVlComercial: TComboBox;
    rbImprimirDadosDocReferenciados: TCheckBox;
    ckImprimeCodigoEan: TCheckBox;
    ChkQuebraLinhaEmDetalhamentos: TCheckBox;
    Label10: TLabel;
    cbPosCanhotoLayout: TComboBox;
    Label11: TLabel;
    cbExibeCampoDePagamento: TComboBox;
    ACBrMDFe1: TACBrMDFe;
    ACBrMDFeDAMDFEFR1: TACBrMDFeDAMDFEFR;
    frxPDFExport1: TfrxPDFExport;
    procedure FormCreate(Sender: TObject);
    procedure btncarregarClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnCarregarEventoClick(Sender: TObject);
  private
    procedure Configuracao;
    procedure Initializao;
      { Private declarations }
  public
      { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
{$IFDEF DELPHIXE6_UP}
  IOUtils,
{$ENDIF}
  pcnConversao;

{$R *.dfm}

procedure TfrmPrincipal.btncarregarClick(Sender: TObject);
begin
  ACBrMDFe1.Manifestos.Clear;
  if OpenDialog1.Execute then
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmPrincipal.btnImprimirClick(Sender: TObject);
begin
  Configuracao;
  if lstbxFR3.ItemIndex = - 1 then
    raise Exception.Create('Selecione um arquivo fr3 ');

  if Pos('damdf', LowerCase(lstbxFR3.Items[ lstbxFR3.ItemIndex ])) > 0 then
  begin
    if ACBrMDFe1.Manifestos.Count = 0 then
      raise Exception.Create('Não foi carregado nenhum xml para impressão');
    ACBrMDFeDAMDFEFR1.FastFile := lstbxFR3.Items[ lstbxFR3.ItemIndex ];

    ACBrMDFe1.Manifestos.Imprimir;
  end
  else
    if Pos('evento', LowerCase(lstbxFR3.Items[ lstbxFR3.ItemIndex ])) > 0 then
    begin
      if ACBrMDFe1.EventoMDFe.Evento.Count = 0 then
        raise Exception.Create('Não tem nenhum evento para imprimir');
      ACBrMDFeDAMDFEFR1.FastFileEvento := lstbxFR3.Items[ lstbxFR3.ItemIndex ];

      ACBrMDFe1.ImprimirEvento;
    end;

end;

procedure TfrmPrincipal.btnCarregarEventoClick(Sender: TObject);
begin
  ACBrMDFe1.Manifestos.Clear;
  ACBrMDFe1.EventoMDFe.Evento.Clear;
  OpenDialog1.Execute();
  ACBrMDFe1.EventoMDFe.LerXML(OpenDialog1.FileName);
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
{$IFDEF DELPHIXE6_UP}
  fsFiles: string;
{$ELSE}
  SR: TSearchRec;
{$ENDIF}
begin
{$IFDEF DELPHIXE6_UP}
  for fsFiles in TDirectory.GetFiles('..\Delphi\Report\') do
    if Pos('.fr3', LowerCase(fsFiles)) > 0 then
      lstbxFR3.AddItem(fsFiles, nil);
{$ELSE}
  if FindFirst('..\Delphi\Report\*.fr3', faArchive, SR) = 0 then
    try
      repeat
        if (SR.Attr and faDirectory) = 0 then
          lstbxFR3.AddItem('..\Delphi\Report\' + SR.Name, nil);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
{$ENDIF}
  Initializao;
end;

procedure TfrmPrincipal.Configuracao;
begin
  ACBrMDFe1.DAMDFE := ACBrMDFeDAMDFEFR1;

    // --- Configurações para MDFe
  With ACBrMDFe1.DAMDFE do
  begin

      // Mostra  a Tarja CANCELADA
    Cancelada := rbTarjaCancelada.Checked;

      // Margens
    MargemEsquerda := StringToFloat(EditMargemEsquerda.Text);
    MargemSuperior := StringToFloat(EditMargemSuperior.Text);
    MargemDireita  := StringToFloat(EditMargemDireita.Text);
    MargemInferior := StringToFloat(EditMargemInferior.Text);

      // Decimais
    CasasDecimais.Formato    := TDetFormato(RgTipodedecimais.ItemIndex);
    CasasDecimais.qCom       := cbtdetInteger_qtd.ItemIndex;
    CasasDecimais.vUnCom     := cbtdetInteger_Vrl.ItemIndex;
    CasasDecimais.MaskqCom   := cbtdetMascara_qtd.Items[ cbtdetMascara_qtd.ItemIndex ];
    CasasDecimais.MaskvUnCom := cbtdetMascara_Vrl.Items[ cbtdetMascara_Vrl.ItemIndex ];
  end;
end;

procedure TfrmPrincipal.Initializao;
begin
  PageControl1.ActivePage := TabArquivos;

  With ACBrMDFeDAMDFEFR1 do
  begin

    EditMargemEsquerda.Text := FloatToString(MargemEsquerda);
    EditMargemSuperior.Text := FloatToString(MargemSuperior);
    EditMargemDireita.Text  := FloatToString(MargemDireita);
    EditMargemInferior.Text := FloatToString(MargemInferior);

    Cancelada := False;

      // Decimais
    RgTipodedecimais.ItemIndex  := integer(CasasDecimais.Formato);
    cbtdetInteger_qtd.ItemIndex := CasasDecimais.qCom;
    cbtdetInteger_Vrl.ItemIndex := CasasDecimais.vUnCom;
    cbtdetMascara_qtd.ItemIndex := CasasDecimais.qCom;
    cbtdetMascara_Vrl.ItemIndex := CasasDecimais.vUnCom;

  end;

  With frxReport1 do
  begin
    ShowProgress := False;
    StoreInDFM   := False;
  end;

end;

end.
