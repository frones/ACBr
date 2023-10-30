{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020   Daniel Simoes de Almeida             }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
{                                                                              }
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

{******************************************************************************
|* Historico
|*
|* 29/03/2012: Isaque Pinheiro / Régys Borges da Silveira
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

{
  Hierarquia de dependência dos Packages
• ACBrComum → Synapse
• ACBrDiversos → ACBrComum
• PCNComum → ACBrDiversos
• ACBrOpenSSL → ACBrComum
• ACBrSerial → ACBrDiversos, ACBrOpenSSL
• ACBrTXTComum → ACBrDiversos,
• ACBrConvenio115 → ACBrTXTComum, ACBrOpenSSL
• ACBrLFD → ACBrTXTComum
• ACBrPAF → ACBrTXTComum, ACBrOpenSSL
• ACBrSEF2 → ACBrTXTComum, PCNComum
• ACBrSintegra → ACBrTXTComum
• ACBrSPED → ACBrTXTComum
• ACBrTCP → ACBrDiversos
• ACBrTEFD → ACBrComum
• ACBr_Boleto → ACBrTCP
• ACBr_BoletoFC_Fortes → ACBr_Boleto, fortes324laz
• ACBr_BoletoFC_LazReport → ACBr_Boleto, lazreportpdfexport
• ACBrDFeComum → ACBrOpenSSL, ACBrTCP, PCNComum
• ACBrNFe → ACBrDFeComum
• ACBrCTe → ACBrDFeComum
• ACBrGNRe → ACBrDFeComum
• ACBrMDFe → ACBrDFeComum
• ACBrNFSe → ACBrDFeComum
• ACBr_SAT → PCNComum
• ACBr_SAT_ECFVirtual → ACBr_SAT, ACBrSerial
• ACBr_SAT_Extrato_ESCPOS → ACBr_SAT, ACBrDFeComum, ACBrSerial
• ACBr_SAT_Extrato_Fortes → ACBr_SAT, ACBrDFeComum, fortes324laz
}

unit uFrameLista;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Buttons,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  ACBrPacotes;

type
  TframePacotes = class(TFrame)
    pnlBotoesMarcar: TPanel;
    btnPacotesDesmarcarTodos: TSpeedButton;
    btnPacotesMarcarTodos: TSpeedButton;
    ScrollBox1: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    ACBr_synapse_dpk: TCheckBox;
    ACBr_Comum_dpk: TCheckBox;
    ACBr_Diversos_dpk: TCheckBox;
    ACBr_Serial_dpk: TCheckBox;
    ACBr_TCP_dpk: TCheckBox;
    ACBr_BPe_dpk: TCheckBox;
    ACBr_TEFD_dpk: TCheckBox;
    ACBr_Boleto_dpk: TCheckBox;
    ACBr_Sintegra_dpk: TCheckBox;
    ACBr_SPED_dpk: TCheckBox;
    ACBr_PAF_dpk: TCheckBox;
    ACBr_OpenSSL_dpk: TCheckBox;
    ACBr_PCNComum_dpk: TCheckBox;
    ACBr_NFe_dpk: TCheckBox;
    ACBr_CTe_dpk: TCheckBox;
    ACBr_NFSe_dpk: TCheckBox;
    ACBr_MDFe_dpk: TCheckBox;
    ACBr_GNRE_dpk: TCheckBox;
    ACBr_Convenio115_dpk: TCheckBox;
    ACBr_SEF2_dpk: TCheckBox;
    ACBr_SAT_dpk: TCheckBox;
    ACBr_NFeDanfeESCPOS_dpk: TCheckBox;
    ACBr_SATExtratoESCPOS_dpk: TCheckBox;
    ACBr_LFD_dpk: TCheckBox;
    ACBr_SPEDImportar_dpk: TCheckBox;
    ACBr_DFeComum_dpk: TCheckBox;
    ACBr_NFCeECFVirtual_dpk: TCheckBox;
    ACBr_SATECFVirtual_dpk: TCheckBox;
    ACBr_TXTComum_dpk: TCheckBox;
    ACBr_DFeReportRL_dpk: TCheckBox;
    ACBr_NFeDanfeFR_dpk: TCheckBox;
    ACBr_CTeDacteFR_dpk: TCheckBox;
    ACBr_NFSeDanfseFR_dpk: TCheckBox;
    ACBr_BoletoFR_dpk: TCheckBox;
    ACBr_MDFeDamdfeFR_dpk: TCheckBox;
    ACBr_GNREGuiaFR_dpk: TCheckBox;
    ACBr_NFeDanfeRL_dpk: TCheckBox;
    ACBr_CTeDacteRL_dpk: TCheckBox;
    ACBr_NFSeDanfseRL_dpk: TCheckBox;
    ACBr_BoletoRL_dpk: TCheckBox;
    ACBr_MDFeDamdfeRL_dpk: TCheckBox;
    ACBr_SATExtratoRL_dpk: TCheckBox;
    ACBr_GNREGuiaRL_dpk: TCheckBox;
    ACBr_BlocoX_dpk: TCheckBox;
    ACBr_DeSTDA_dpk: TCheckBox;
    ACBr_Ponto_dpk: TCheckBox;
    ACBr_MTER_dpk: TCheckBox;
    ACBr_SATWS_dpk: TCheckBox;
    ACBr_ANe_dpk: TCheckBox;
    ACBr_Integrador_dpk: TCheckBox;
    ACBre_Social_dpk: TCheckBox;
    ACBr_Reinf_dpk: TCheckBox;
    ACBr_BPeDabpeESCPOS_dpk: TCheckBox;
    Label27: TLabel;
    ACBr_CIOT_dpk: TCheckBox;
    ACBr_LCDPR_dpk: TCheckBox;
    ACBr_ONE_dpk: TCheckBox;
    ACBr_EDI_dpk: TCheckBox;
    ACBr_NF3e_dpk: TCheckBox;
    ACBr_NF3eDANF3eESCPOS_dpk: TCheckBox;
    ACBr_ADRCST_dpk: TCheckBox;
    Label28: TLabel;
    ACBr_SATExtratoFR_dpk: TCheckBox;
    ACBr_PagFor_dpk: TCheckBox;
    ACBr_NFSeX_dpk: TCheckBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    ACBr_NFSeXDanfseRL_dpk: TCheckBox;
    Label29: TLabel;
    ACBr_NFSeXDanfseFR_dpk: TCheckBox;
    ACBr_OFX_dpk: TCheckBox;
    ACBr_GTIN_dpk: TCheckBox;
    ACBr_OpenDelivery_dpk: TCheckBox;
    ACBr_PAFNFCe_dpk: TCheckBox;
    ACBr_PIXCD_dpk: TCheckBox;
    ACBr_Android_dpk: TCheckBox;
    lblacb: TLabel;
    lblSubTituloFPDF: TLabel;
    lblFPDF_BoletoDPK: TLabel;
    ACBr_BoletoFPDF_dpk: TCheckBox;
    ACBr_DebitoAutomatico_dpk: TCheckBox;
    Label30: TLabel;
    ACBr_NFeDanfeFPDF_dpk: TCheckBox;
    Label31: TLabel;
    ACBr_OutrosDFeTCP_dpk: TCheckBox;
    Label32: TLabel;
    procedure btnPacotesMarcarTodosClick(Sender: TObject);
    procedure btnPacotesDesmarcarTodosClick(Sender: TObject);
    procedure VerificarCheckboxes(Sender: TObject);
  private
    FPacotes: TPacotes;
    FUtilizarBotoesMarcar: Boolean;
    procedure AtualizarTelaConformeListaDePacotes(ListaPacotes: TPacotes);
    procedure AtualizarListaDePacotesConformeTela(ListaPacotes: TPacotes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CarregarDeArquivoIni(const ArquivoIni: string);
    procedure SalvarEmArquivoIni(const ArquivoIni: string);
    property Pacotes: TPacotes read FPacotes write FPacotes;
  end;

implementation

uses
  StrUtils,
  IniFiles;

{$R *.dfm}

procedure TframePacotes.CarregarDeArquivoIni(const ArquivoIni: string);
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(ArquivoIni);
  try
    //Não usar ArqIni.ReadSection porque pode ser que houveram mudanças nos pacotes...
    for I := 0 to Pacotes.Count - 1 do
      Pacotes[I].MarcadoParaInstalar := ArqIni.ReadBool('PACOTES', Pacotes[I].GetNome, False);
  finally
    ArqIni.Free;
  end;

  AtualizarTelaConformeListaDePacotes(Pacotes);

end;

procedure TframePacotes.SalvarEmArquivoIni(const ArquivoIni: string);
var
  ArqIni: TIniFile;
  I: Integer;
begin
  AtualizarListaDePacotesConformeTela(Pacotes);

  ArqIni := TIniFile.Create(ArquivoIni);
  try
    ArqIni.EraseSection('PACOTES');
    for I := 0 to Pacotes.Count - 1 do
      ArqIni.WriteBool('PACOTES', Pacotes[I].GetNome, Pacotes[I].MarcadoParaInstalar);
  finally
    ArqIni.Free;
  end;

end;

procedure TframePacotes.AtualizarTelaConformeListaDePacotes(ListaPacotes: TPacotes);
var
  I: Integer;
  j: Integer;
  achkBox: TCheckBox;
begin
  for I := 0 to ListaPacotes.Count - 1 do
  begin
    for j := 0 to ScrollBox1.ControlCount  - 1 do
    begin
      if (ScrollBox1.Controls[j] is TCheckBox) then
      begin
        achkBox := (ScrollBox1.Controls[j] as TCheckBox);
        if achkBox.Caption = ListaPacotes[i].GetNome then
          achkBox.Checked := ListaPacotes[i].MarcadoParaInstalar;
      end;
    end;
  end;
end;

procedure TframePacotes.AtualizarListaDePacotesConformeTela(ListaPacotes: TPacotes);
var
  I: Integer;
  j: Integer;
  achkBox: TCheckBox;
begin
  for I := 0 to ListaPacotes.Count - 1 do
  begin
    for j := 0 to ScrollBox1.ControlCount - 1 do
    begin
      if (ScrollBox1.Controls[j] is TCheckBox) then
      begin
        achkBox := (ScrollBox1.Controls[j] as TCheckBox);
        if achkBox.Caption = ListaPacotes[i].GetNome then
        begin
          ListaPacotes[i].MarcadoParaInstalar := achkBox.Checked;
          Break;
        end;
      end;
    end;
  end;
end;

constructor TframePacotes.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  // variavel para controle do verificar checkboxes
  // utilizada para evitar estouro de pilha por conta da redundância
  // e também para que pacotes dependentes não atrapalhem a rotina
  FUtilizarBotoesMarcar := False;

  // lista de pacotes (checkboxes) disponiveis
  FPacotes := TPacotes.Create;

  // popular a lista de pacotes com os pacotes disponíveis
  // colocar todos os checkboxes disponíveis na lista
  FPacotes.Clear;
  for I := 0 to Self.ComponentCount - 1 do
  begin
    if Self.Components[I] is TCheckBox then
       FPacotes.Add(TPacote.Create(TCheckBox(Self.Components[I])));
  end;
  FPacotes.Sort(TComparer<TPacote>.Construct(
      function(const Dpk1, Dpk2: TPacote): Integer
      begin
         Result := CompareStr( FormatFloat('0000', Dpk1.OrdemListagem), FormatFloat('0000', Dpk2.OrdemListagem) );
      end));
end;

destructor TframePacotes.Destroy;
begin
  FreeAndNil(FPacotes);

  inherited;
end;

// botão para marcar todos os checkboxes
procedure TframePacotes.btnPacotesMarcarTodosClick(Sender: TObject);
var
  I: Integer;
  vCheckbox: TCheckBox;
begin
  FUtilizarBotoesMarcar := True;
  try
    for I := 0 to Self.ComponentCount -1 do
    begin
      if Self.Components[I] is TCheckBox then
      begin
        vCheckbox := TCheckBox(Self.Components[I]);
        if vCheckbox.Enabled and vCheckbox.Visible  then
          vCheckbox.Checked := True;
      end;
    end;
  finally
    FUtilizarBotoesMarcar := False;
    VerificarCheckboxes(Sender);
  end;
end;

// botão para desmarcar todos os checkboxes
procedure TframePacotes.btnPacotesDesmarcarTodosClick(Sender: TObject);
var
  I: Integer;
  vCheckbox: TCheckBox;
begin
  FUtilizarBotoesMarcar := True;
  try
    for I := 0 to Self.ComponentCount -1 do
    begin
      if Self.Components[I] is TCheckBox then
      begin
        vCheckbox := TCheckBox(Self.Components[I]);
        if vCheckbox.Enabled and vCheckbox.Visible  then
          vCheckbox.Checked := False;
      end;
    end;
  finally
    FUtilizarBotoesMarcar := False;
    VerificarCheckboxes(Sender);
  end;
end;

procedure TframePacotes.VerificarCheckboxes(Sender: TObject);
begin
// rotina de verificação de dependência e marcação dos pacotes base

  //If necessário para evitar stackoverflow
  /// caso alguma alteração abaixo dispare esse evento novamente
  if not FUtilizarBotoesMarcar then
  begin
    FUtilizarBotoesMarcar := True;

    try
      // pacotes base não podem ser desmarcados
      // instalação mínima do ACBr
      ACBr_synapse_dpk.Checked  := True;
      ACBr_Comum_dpk.Checked    := True;
      ACBr_Diversos_dpk.Checked := True;

      //1) Primeiro verificar dependências das impressões marcadas...

      // quando não for selecionado o NFe devemos desmarcar
      if not ACBr_NFe_dpk.Checked then
      begin
        ACBr_NFeDanfeFR_dpk.Checked := False;
        ACBr_NFeDanfeRL_dpk.Checked := False;
      end;

      // quando não for selecionado o CTe devemos desmarcar
      if not ACBr_CTe_dpk.Checked then
      begin
        ACBr_CTeDacteFR_dpk.Checked := False;
        ACBr_CTeDacteRL_dpk.Checked := False;
      end;

      // quando não for selecionado o NFSe devemos desmarcar
      if not ACBr_NFSe_dpk.Checked then
      begin
        ACBr_NFSeDanfseFR_dpk.Checked := False;
        ACBr_NFSeDanfseRL_dpk.Checked := False;
      end;

      // quando não for selecionado o NFSeX devemos desmarcar
      if not ACBr_NFSeX_dpk.Checked then
      begin
        ACBr_NFSeXDanfseFR_dpk.Checked := False;
        ACBr_NFSeXDanfseRL_dpk.Checked := False;
      end;

      // quando não for selecionado o Boleto devemos desmarcar
      if not ACBr_Boleto_dpk.Checked then
      begin
        ACBr_BoletoFR_dpk.Checked := False;
        ACBr_BoletoRL_dpk.Checked := False;
        ACBr_BoletoFPDF_dpk.Checked := False;
      end;

      // quando não for selecionado o MDF-e devemos desmarcar
      if not ACBr_MDFe_dpk.Checked then
      begin
        ACBr_MDFeDamdfeFR_dpk.Checked := False;
        ACBr_MDFeDamdfeRL_dpk.Checked := False;
      end;

      // quando não for selecionado o SAT devemos desmarcar
      if not ACBr_SAT_dpk.Checked then
      begin
        ACBr_SATExtratoRL_dpk.Checked := False;
        ACBr_SATExtratoFR_dpk.Checked := False;
      end;

      // quando não for selecionado o GNRE devemos desmarcar
      if not ACBr_GNRE_dpk.Checked then
      begin
        ACBr_GNREGuiaFR_dpk.Checked := False;
        ACBr_GNREGuiaRL_dpk.Checked := False;
      end;

      // dependencia do NFe
      if ACBr_NFeDanfeESCPOS_dpk.Checked and
        (not(ACBr_NFe_dpk.Checked) or not(ACBr_Serial_dpk.Checked)) then
      begin
        ACBr_Serial_dpk.Checked := True;
        ACBr_NFe_dpk.Checked    := True;
      end;

      // dependencia do BPe
      if ACBr_BPeDabpeESCPOS_dpk.Checked and
        (not(ACBr_BPe_dpk.Checked) or not(ACBr_Serial_dpk.Checked)) then
      begin
        ACBr_Serial_dpk.Checked := True;
        ACBr_BPe_dpk.Checked    := True;
      end;

      // dependencia da NF3e
      if ACBr_NF3eDANF3eESCPOS_dpk.Checked and
        (not(ACBr_NF3e_dpk.Checked) or not(ACBr_Serial_dpk.Checked)) then
      begin
        ACBr_Serial_dpk.Checked := True;
        ACBr_NF3e_dpk.Checked   := True;
      end;

      // dependencia do SAT
      if ACBr_SATExtratoESCPOS_dpk.Checked and
        (not(ACBr_SAT_dpk.Checked) or not(ACBr_Serial_dpk.Checked)) then
      begin
        ACBr_Serial_dpk.Checked := True;
        ACBr_SAT_dpk.Checked    := True;
      end;

      if (ACBr_SATExtratoRL_dpk.Checked or
          ACBr_SATExtratoFR_dpk.Checked)
        and not(ACBr_SAT_dpk.Checked) then
        ACBr_SAT_dpk.Checked := True;

      //2) Segundo Verificar dependência de outros pacotes marcados...

      //
      if (ACBr_NFe_dpk.Checked)    or (ACBr_CTe_dpk.Checked)   or
         (ACBr_NFSe_dpk.Checked)   or (ACBr_MDFe_dpk.Checked)  or
         (ACBr_BlocoX_dpk.Checked) or (ACBr_SATWS_dpk.Checked) or
         (ACBr_BPe_dpk.Checked)    or (ACBr_ANe_dpk.Checked)   or
         (ACBr_CIOT_dpk.Checked)   or (ACBr_NF3e_dpk.Checked)  or
         (ACBr_SAT_dpk.Checked)    or (ACBr_Boleto_dpk.Checked) or
         (ACBr_PAFNFCe_dpk.Checked) then
      begin
        ACBr_DFeComum_dpk.Checked := True;
      end;

      if ACBr_DFeComum_dpk.Checked then
      begin
        ACBr_Integrador_dpk.Checked := True;
        ACBr_PCNComum_dpk.Checked   := True;
        ACBr_OpenSSL_dpk.Checked    := True;
        ACBr_TCP_dpk.Checked        := True;
      end;

      if (ACBr_TEFD_dpk.Checked or ACBr_MTER_dpk.Checked) then
        ACBr_TCP_dpk.Checked := True;

      if ACBr_SEF2_dpk.Checked then
      begin
        ACBr_TXTComum_dpk.Checked := True;
        ACBr_PCNComum_dpk.Checked := True;
      end;

      // Dependencias do ACBrPaf
      if ACBr_PAF_dpk.Checked then
      begin
        ACBr_TXTComum_dpk.Checked := True;
        ACBr_OpenSSL_dpk.Checked  := True;
      end;

      if ACBr_SPED_dpk.Checked or ACBr_Sintegra_dpk.Checked or
         ACBr_Convenio115_dpk.Checked or ACBr_DeSTDA_dpk.Checked or
         ACBr_EDI_dpk.Checked or ACBr_LCDPR_dpk.Checked or
         ACBr_LFD_dpk.Checked or ACBr_Ponto_dpk.Checked or
         ACBr_SPEDImportar_dpk.Checked or ACBr_ADRCST_dpk.Checked then
      begin
        ACBr_TXTComum_dpk.Checked := True;
      end;

      if (not ACBr_DFeReportRL_dpk.Checked) and
         (ACBr_NFeDanfeRL_dpk.Checked or ACBr_NFSeDanfseRL_dpk.Checked or
          ACBr_CTeDacteRL_dpk.Checked or ACBr_BoletoRL_dpk.Checked or
          ACBr_MDFeDamdfeRL_dpk.Checked or ACBr_SATExtratoRL_dpk.Checked or
          ACBr_GNREGuiaRL_dpk.Checked or ACBr_NFSeXDanfseRL_dpk.Checked) then
      begin
        ACBr_DFeReportRL_dpk.Checked := True;
      end;

      // Dependencias do OpenDelivery
      if ACBr_OpenDelivery_dpk.Checked then
      begin
        ACBr_OpenSSL_dpk.Checked  := True;
      end;

      if ACBr_PIXCD_dpk.Checked then
      begin
        ACBr_OpenSSL_dpk.Checked  := True;
      end;

      if ACBr_Boleto_dpk.Checked then
      begin
        ACBr_PIXCD_dpk.Checked := True;
      end;

    finally
      FUtilizarBotoesMarcar := False;
    end;
  end;
end;

end.
