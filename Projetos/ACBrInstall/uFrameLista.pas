{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 29/03/2012: Isaque Pinheiro / Régys Borges da Silveira
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}
unit uFrameLista;

interface

uses
  Generics.Collections,
  
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Buttons, ExtCtrls, StdCtrls, ComCtrls;

type
  TPacotes = TList<TCheckBox>;

  TframePacotes = class(TFrame)
    pnlBotoesPacotes: TPanel;
    btnPacotesDesmarcarTodos: TSpeedButton;
    btnPacotesMarcarTodos: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ACBr_synapse_dpk: TCheckBox;
    ACBrComum_dpk: TCheckBox;
    ACBrDiversos_dpk: TCheckBox;
    ACBrSerial_dpk: TCheckBox;
    ACBrTCP_dpk: TCheckBox;
    ACBr_TEFD_dpk: TCheckBox;
    ACBr_Boleto_dpk: TCheckBox;
    ACBr_Sintegra_dpk: TCheckBox;
    ACBr_SPED_dpk: TCheckBox;
    ACBr_PAF_dpk: TCheckBox;
    ACBrOpenSSL_dpk: TCheckBox;
    ACBrCapicom_dpk: TCheckBox;
    PCN2_dpk: TCheckBox;
    ACBr_NFe2_dpk: TCheckBox;
    ACBr_CTe_dpk: TCheckBox;
    Label8: TLabel;
    ACBr_NFSe_dpk: TCheckBox;
    Label18: TLabel;
    ACBr_MDFe_dpk: TCheckBox;
    ACBr_LFD_dpk: TCheckBox;
    PageControl1: TPageControl;
    tsNFe: TTabSheet;
    tsCTe: TTabSheet;
    tsNFSe: TTabSheet;
    tsBoletos: TTabSheet;
    tsMDFe: TTabSheet;
    tsSAT: TTabSheet;
    tsGNRE: TTabSheet;
    ACBrNFeDanfeFR_dpk: TCheckBox;
    ACBrNFeDanfeRL_dpk: TCheckBox;
    ACBrCTeDacteFR_dpk: TCheckBox;
    ACBrCTeDacteRLpkg: TCheckBox;
    ACBrNFSeDanfseRLpkg_dpk: TCheckBox;
    ACBrNFSeDanfseFRpkg_dpk: TCheckBox;
    ACBr_BoletoFC_FR_dpk: TCheckBox;
    ACBr_BoletoFC_Fortes_dpk: TCheckBox;
    Label9: TLabel;
    Label11: TLabel;
    ACBr_GNRE_dpk: TCheckBox;
    ACBr_Convenio115_dpk: TCheckBox;
    ACBr_SEF2_dpk: TCheckBox;
    ACBr_SAT_dpk: TCheckBox;
    ACBrMDFeDAMDFEFRpkg_dpk: TCheckBox;
    ACBrMDFeDAMDFeRLpkg_dpk: TCheckBox;
    ACBr_SAT_Extrato_Fortes_dpk: TCheckBox;
    ACBrGNREGuiaFRpkg_dpk: TCheckBox;
    ACBrNFeDanfeESCPOS_dpk: TCheckBox;
	ACBr_SAT_Extrato_ESCPOS_dpk: TCheckBox;
	ACBrGNREGuiaRLpkg_dpk: TCheckBox;
    ACBr_SPED_Importar_dpk: TCheckBox;
    procedure btnPacotesMarcarTodosClick(Sender: TObject);
    procedure btnPacotesDesmarcarTodosClick(Sender: TObject);
    procedure VerificarCheckboxes(Sender: TObject);
    procedure tsNFeHide(Sender: TObject);
  private
    FPacotes: TPacotes;
    FUtilizarBotoesMarcar: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsPacoteNF2(const ANomePacote: String): Boolean;
    function IsPacoteCTe(const ANomePacote: String): Boolean;
    function IsPacoteNFSe(const ANomePacote: String): Boolean;
    function IsPacoteMDFe(const ANomePacote: String): Boolean;
    function IsPacoteBoleto(const ANomePacote: String): Boolean;
    function IsPacoteSped(const ANomePacote: String): Boolean;
    function IsPacoteSintegra(const ANomePacote: String): Boolean;
    function IsPacotePaf(const ANomePacote: String): Boolean;
    function IsPacoteSerial(const ANomePacote: String): Boolean;
    function IsPacoteTEFD(const ANomePacote: String): Boolean;
    function IsPacoteSAT(const ANomePacote: String): Boolean;
    function IsPacoteGNRE(const ANomePacote: String): Boolean;

    property Pacotes: TPacotes read FPacotes write FPacotes;
  end;

implementation

uses
  StrUtils;

{$R *.dfm}

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
    begin
      if TCheckBox(Self.Components[I]).Tag = 0 then
        FPacotes.Add(TCheckBox(Self.Components[I]));
    end;
  end;
end;

destructor TframePacotes.Destroy;
begin
  FreeAndNil(FPacotes);

  inherited;
end;

function TframePacotes.IsPacoteBoleto(const ANomePacote: String): Boolean;
const
  PACOTES_BLT: array [0..3] of String =
    ('ACBr_Boleto.dpk',
     'ACBr_BoletoFC_Fortes.dpk',
     'ACBr_BoletoFC_Quick.dpk',
     'ACBr_BoletoFC_FR.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_BLT);
end;

function TframePacotes.IsPacoteCTe(const ANomePacote: String): Boolean;
const
  PACOTES_CTe: array [0..3] of String =
    ('ACBr_CTe.dpk',
     'ACBrCTeDacteFRpkg.dpk',
     'ACBrCTeDacteQRpkg.dpk',
     'ACBrCTeDacteRLpkg.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_CTe);
end;

function TframePacotes.IsPacoteGNRE(const ANomePacote: String): Boolean;
const
  PACOTES_GNRE: array [0..2] of String =
    ('ACBr_GNRE.dpk',
    'ACBrGNREGuiaFRpkg.dpk',
	'ACBrGNREGuiaRLpkg.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_GNRE);
end;

function TframePacotes.IsPacoteNFSe(const ANomePacote: String): Boolean;
const
  PACOTES_NFSe: array [0..4] of String =
    ('ACBr_NFSe.dpk',
     'ACBrNFSeDanfseFRpkg.dpk',
     'ACBrNFSeDanfseRLpkg.dpk',
     'ACBrNFSeDanfseQRpkg.dpk',
	 'ACBrNFSeDANFSeRVpkg.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_NFSe);
end;

function TframePacotes.IsPacoteSAT(const ANomePacote: String): Boolean;
const
  PACOTES_SAT: array [0..2] of String =
    ('ACBr_SAT.dpk',
	'ACBr_SAT_Extrato_ESCPOS.dpk',
    'ACBr_SAT_Extrato_Fortes.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_SAT);
end;

function TframePacotes.IsPacoteSerial(const ANomePacote: String): Boolean;
const
  PACOTES_SERIAL: array [0..1] of String =
    ('ACBrSerial.dpk',
     'ACBrTCP.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_SERIAL);
end;

function TframePacotes.IsPacoteSintegra(const ANomePacote: String): Boolean;
const
  PACOTES_STG: array [0..0] of String =
    ('ACBr_Sintegra.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_STG);
end;

function TframePacotes.IsPacotePaf(const ANomePacote: String): Boolean;
const
  PACOTES_PAF: array [0..0] of String =
    ('ACBr_PAF.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_PAF);
end;

function TframePacotes.IsPacoteSped(const ANomePacote: String): Boolean;
const
  PACOTES_SPED: array [0..4] of String =
    ('ACBr_SPED.dpk',
     'ACBr_SPED_Importar.dpk',
     'ACBr_LFD.dpk',
     'ACBr_SEF2.dpk',
     'ACBr_Convenio115.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_SPED);
end;

function TframePacotes.IsPacoteTEFD(const ANomePacote: String): Boolean;
const
  PACOTES_TEFD: array [0..0] of String =
    ('ACBr_TEFD.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_TEFD);
end;

procedure TframePacotes.tsNFeHide(Sender: TObject);
var
  i :integer;
begin
  for i := 0 to TTabSheet(Sender).ComponentCount -1 do
    if (TTabSheet(Sender).Components[i] is TCheckBox) then
      (TTabSheet(Sender).Components[i] as TCheckBox).Checked := false;
end;

function TframePacotes.IsPacoteMDFe(const ANomePacote: String): Boolean;
const
  PACOTES_MDFe: array [0..3] of String =
    ('ACBr_MDFe.dpk',
     'ACBrMDFeDAMDFEFRpkg.dpk',
     'ACBrMDFeDAMDFEQRpkg.dpk',
     'ACBrMDFeDAMDFeRLpkg.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_MDFe);
end;

function TframePacotes.IsPacoteNF2(const ANomePacote: String): Boolean;
const
  PACOTES_NF2: array [0..6] of String =
    ('ACBr_NFe2.dpk',
     'ACBrNFeDanfeFRpkg.dpk',
     'ACBrNFeDanfeRV.dpk',
     'ACBrNFeDanfeRVCodeBase.dpk',
     'ACBrNFeDanfeRLpkg.dpk',
     'ACBrNFeDanfeQRpkg.dpk',
     'ACBrNFeDanfeESCPOS.dpk');
begin
  Result := MatchText(ANomePacote, PACOTES_NF2);
end;

// botão para marcar todos os checkboxes
procedure TframePacotes.btnPacotesMarcarTodosClick(Sender: TObject);
var
  I: Integer;
begin
  FUtilizarBotoesMarcar := True;
  try
    for I := 0 to Self.ComponentCount -1 do
    begin
      if Self.Components[I] is TCheckBox then
      begin
        if TCheckBox(Self.Components[I]).Enabled then
          TCheckBox(Self.Components[I]).Checked := True;
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
begin
  FUtilizarBotoesMarcar := True;
  try
    for I := 0 to Self.ComponentCount -1 do
    begin
      if Self.Components[I] is TCheckBox then
      begin
        if TCheckBox(Self.Components[I]).Enabled then
          TCheckBox(Self.Components[I]).Checked := False;
      end;
    end;
  finally
    FUtilizarBotoesMarcar := False;
    VerificarCheckboxes(Sender);
  end;
end;

// rotina de verificação de dependência e marcação dos pacotes base
procedure TframePacotes.VerificarCheckboxes(Sender: TObject);
begin
  // pacotes base não podem ser desmarcados
  // instalação mínima do ACBr
  ACBr_synapse_dpk.Checked      := True;
  ACBrComum_dpk.Checked    := True;
  ACBrDiversos_dpk.Checked := True;

  if not FUtilizarBotoesMarcar then
  begin
	FUtilizarBotoesMarcar := True;/// caso algum evento abaixo dispare novamente
	try
  		// quando não for selecionado o NFe devemos desmarcar
		if not ACBr_NFe2_dpk.Checked then
		begin
      ACBrNFeDanfeFR_dpk.Checked := False;
      ACBrNFeDanfeRL_dpk.Checked := False;
		end;
  		// quando não for selecionado o CTe devemos desmarcar
		if not ACBr_CTe_dpk.Checked then
		begin
      ACBrCTeDacteFR_dpk.Checked := False;
      ACBrCTeDacteRLpkg.Checked := False;
		end;
  		// quando não for selecionado o NFSe devemos desmarcar
		if not ACBr_NFSe_dpk.Checked then
		begin
      ACBrNFSeDanfseFRpkg_dpk.Checked := False;
      ACBrNFSeDanfseRLpkg_dpk.Checked := False;
		end;
		// quando não for selecionado o Boleto devemos desmarcar
		if not ACBr_Boleto_dpk.Checked then
		begin
		  ACBr_BoletoFC_FR_dpk.Checked := False;
		  ACBr_BoletoFC_Fortes_dpk.Checked := False;
		end;
		// quando não for selecionado o MDF-e devemos desmarcar
		if not ACBr_MDFe_dpk.Checked then
		begin
		  ACBrMDFeDAMDFEFRpkg_dpk.Checked := False;
		  ACBrMDFeDAMDFeRLpkg_dpk.Checked := False;
		end;
		// quando não for selecionado o SAT devemos desmarcar
		if not ACBr_SAT_dpk.Checked then
		begin
		  ACBr_SAT_Extrato_Fortes_dpk.Checked := False;
		end;
		// quando não for selecionado o GNRE devemos desmarcar
		if not ACBr_GNRE_dpk.Checked then
		begin
		  ACBrGNREGuiaFRpkg_dpk.Checked := False;
    ACBrGNREGuiaRLpkg_dpk.Checked := False;
		end;

		  // dependencia do NFe
		if ACBrNFeDanfeESCPOS_dpk.Checked and (not (ACBr_NFe2_dpk.Checked) or not(ACBrSerial_dpk.Checked))  then
		begin
		  ACBrSerial_dpk.Checked := true;
		  ACBr_NFe2_dpk.Checked := true;
		end;

		  // dependencia do SAT
		if ACBr_SAT_Extrato_ESCPOS_dpk.Checked and (not(ACBr_SAT_dpk.Checked) or not(ACBrSerial_dpk.Checked)) then
		begin
		  ACBrSerial_dpk.Checked := true;
		  ACBr_SAT_dpk.Checked := true;
		end;

		if (ACBr_SAT_Extrato_Fortes_dpk.Checked) and not(ACBr_SAT_dpk.Checked) then
		  ACBr_SAT_dpk.Checked := true;

    if ACBr_SAT_dpk.Checked and not(PCN2_dpk.Checked) then
      PCN2_dpk.Checked := True;

		// dependencias da NFe e CTe
		if (ACBr_NFe2_dpk.Checked) or (ACBr_CTe_dpk.Checked) or (ACBr_NFSe_dpk.Checked) or (ACBr_MDFe_dpk.Checked) then
		begin
		  PCN2_dpk.Checked        := True;
		  ACBrCapicom_dpk.Checked := True;
		  ACBrOpenSSL_dpk.Checked := True;
		end;

		// dependencias do ACBrTEFD
		if not(ACBrTCP_dpk.Checked) and ACBr_TEFD_dpk.Checked  then
		  ACBrTCP_dpk.Checked := True;

		// Dependencias do ACBrPaf
		if not(ACBr_SPED_dpk.Checked) and ACBr_PAF_dpk.Checked then
		  ACBr_SPED_dpk.Checked := True;
	finally
	  FUtilizarBotoesMarcar := false;
	end;
  end;
  tsNFe.TabVisible := ACBr_NFe2_dpk.Checked;
  tsCTe.TabVisible := ACBr_CTe_dpk.Checked;
  tsNFSe.TabVisible := ACBr_NFSe_dpk.Checked;
  tsBoletos.TabVisible := ACBr_Boleto_dpk.Checked;
  tsMDFe.TabVisible := ACBr_MDFe_dpk.Checked;
  tsSAT.TabVisible := ACBr_SAT_dpk.Checked;
  tsGNRE.TabVisible := ACBr_GNRE_dpk.Checked;

end;

end.

{
  --------------------------------------
  -- LEMBRETE NÃO APAGAR POR ENQUANTO --
  --------------------------------------
  synapse.dpk
  ACBrComum.dpk
  ACBrDiversos.dpk

  ACBrSerial.dpk
  ACBrTCP.dpk
  ACBr_TEFD.dpk

  ACBr_Boleto.dpk
  ACBr_PAF.dpk
  ACBr_Sintegra.dpk
  ACBr_SPED.dpk

  ACBrCapicom.dpk
  ACBrOpenSSL.dpk
  ACBrNFe2\PCN2.dpk
  ACBrNFe2\ACBr_NFe2.dpk
  ACBrNFe2\ACBr_CTe.dpk

}
