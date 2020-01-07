{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Daniel Simoes de Almeida             }
{                                         Isaque Pinheiro                      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 29/03/2012: Isaque Pinheiro / Régys Borges da Silveira
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}
unit uPrincipal;

interface

uses
  Windows, Messages, FileCtrl, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, pngimage,
  IOUtils, UITypes, JclIDEUtils, JclCompilerUtils,
  Types, JvComponentBase, JvCreateProcess, JvExControls, JvAnimatedImage,
  JvGIFCtrl, JvWizard, JvWizardRouteMapNodes, CheckLst,
  uFrameLista, ACBrUtil, ACBrPacotes;

type

  TfrmPrincipal = class(TForm)
    wizPrincipal: TJvWizard;
    wizMapa: TJvWizardRouteMapNodes;
    wizPgConfiguracao: TJvWizardInteriorPage;
    wizPgInstalacao: TJvWizardInteriorPage;
    wizPgFinalizar: TJvWizardInteriorPage;
    wizPgInicio: TJvWizardWelcomePage;
    Label2: TLabel;
    edtDirDestino: TEdit;
    Label6: TLabel;
    imgLogomarca: TImage;
    lstMsgInstalacao: TListBox;
    pnlTopo: TPanel;
    Label9: TLabel;
    btnSelecDirInstall: TSpeedButton;
    imgGifPropagandaACBrSAC: TJvGIFAnimator;
    Label3: TLabel;
    pgbInstalacao: TProgressBar;
    lblUrlACBrSac1: TLabel;
    lblUrlForum1: TLabel;
    lblUrlACBr1: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label7: TLabel;
    btnInstalarACBr: TSpeedButton;
    btnVisualizarLogCompilacao: TSpeedButton;
    pnlInfoCompilador: TPanel;
    wizPgPacotes: TJvWizardInteriorPage;
    rdgDLL: TRadioGroup;
    ckbCopiarTodasDll: TCheckBox;
    ckbBCB: TCheckBox;
    lbInfo: TListBox;
    Label8: TLabel;
    chkDeixarSomenteLIB: TCheckBox;
    ckbRemoverArquivosAntigos: TCheckBox;
    JvCreateProcess1: TJvCreateProcess;
    Label22: TLabel;
    ckbRemoveOpenSSL: TCheckBox;
    ckbRemoveCapicom: TCheckBox;
    ckbCargaDllTardia: TCheckBox;
    ckbRemoverCastWarnings: TCheckBox;
    ckbUsarArquivoConfig: TCheckBox;
    framePacotes1: TframePacotes;
    ckbRemoveXMLSec: TCheckBox;
    wizPgSelectIDEs: TJvWizardInteriorPage;
    clbDelphiVersion: TCheckListBox;

    procedure imgPropaganda1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure wizPgInicioNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure URLClick(Sender: TObject);
    procedure btnSelecDirInstallClick(Sender: TObject);
    procedure wizPrincipalCancelButtonClick(Sender: TObject);
    procedure wizPrincipalFinishButtonClick(Sender: TObject);
    procedure wizPgConfiguracaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnInstalarACBrClick(Sender: TObject);
    procedure wizPgInstalacaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnVisualizarLogCompilacaoClick(Sender: TObject);
    procedure wizPgInstalacaoEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure clbDelphiVersionClick(Sender: TObject);
    procedure wizPgPacotesNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure wizPgSelectIDEsNextButtonClick(Sender: TObject; var Stop: Boolean);
  private
    FoACBr: TJclBorRADToolInstallations;
    FUltimoArquivoLog: string;

    procedure GravarConfiguracoesEmArquivoIni;
    procedure LerConfiguracoes;
    function PathArquivoIni: String;
    procedure ValidarSeExistemPacotesNasPastas(var Stop: Boolean; const PastaACBr: string; ListaPacotes:
        TPacotes);
    function ProcedeInstalacao: Boolean;
    procedure AbrirArquivoLogAtual;

    procedure IncrementaBarraProgresso;
    procedure Logar(const AString: String);
    procedure IniciaNovaInstalacao(const MaximoPassosProgresso: Integer; const NomeCaminhoArquivoLog: string;
          const Cabecalho: string);
    procedure MontaListaIDEsSuportadas;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  ShellApi, IniFiles, StrUtils, Math, Registry, ACBrInstallUtils, Generics.Collections,
  ACBrInstallDelphiComponentes, DelphiData;

{$R *.dfm}

// retornar o caminho completo para o arquivo .ini de configurações
function TfrmPrincipal.PathArquivoIni: String;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TfrmPrincipal.ValidarSeExistemPacotesNasPastas(var Stop: Boolean; const PastaACBr: string;
      ListaPacotes: TPacotes);
var
  I: Integer;
  NomePacote: string;
  sDirPackage: string;
begin
  // verificar se os pacotes existem antes de seguir para o próximo passo
  for I := 0 to ListaPacotes.Count - 1 do
  begin
    if ListaPacotes[I].Checked then
    begin
      NomePacote := ListaPacotes[I].Caption;
      // Busca diretório do pacote
      sDirPackage := FindDirPackage(IncludeTrailingPathDelimiter(PastaACBr) + 'Pacotes\Delphi', NomePacote);
      if Trim(sDirPackage) = '' then
        raise Exception.Create('Não foi possível retornar o diretório do pacote : ' + NomePacote);
      if IsDelphiPackage(NomePacote) then
      begin
        if not FileExists(IncludeTrailingPathDelimiter(sDirPackage) + NomePacote) then
        begin
          Stop := True;
          Application.MessageBox(PWideChar(Format('Pacote "%s" não encontrado, efetue novamente o download do repositório', [NomePacote])), 'Erro.', MB_ICONERROR + MB_OK);
          Break;
        end;
      end;
    end;
  end;
end;

// ler o arquivo .ini de configurações e setar os campos com os valores lidos
procedure TfrmPrincipal.LerConfiguracoes;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    edtDirDestino.Text             := ArqIni.ReadString('CONFIG', 'DiretorioInstalacao', ExtractFilePath(ParamStr(0)));
    rdgDLL.ItemIndex               := ArqIni.ReadInteger('CONFIG','DestinoDLL', 0);
    ckbCopiarTodasDll.Checked      := True;
    ckbBCB.Checked                 := ArqIni.ReadBool('CONFIG','C++Builder', False);
    chkDeixarSomenteLIB.Checked    := ArqIni.ReadBool('CONFIG','DexarSomenteLib', False);
    ckbRemoveOpenSSL.Checked       := ArqIni.ReadBool('CONFIG','RemoveOpenSSL', False);
    ckbRemoveCapicom.Checked       := ArqIni.ReadBool('CONFIG','RemoveCapicom', False);
    ckbRemoveXMLSec.Checked        := True;
    ckbCargaDllTardia.Checked      := ArqIni.ReadBool('CONFIG','CargaDllTardia', False);
    ckbRemoverCastWarnings.Checked := ArqIni.ReadBool('CONFIG','RemoverCastWarnings', False);
    ckbUsarArquivoConfig.Checked   := True;

    for I := 0 to framePacotes1.Pacotes.Count - 1 do
      framePacotes1.Pacotes[I].Checked := ArqIni.ReadBool('PACOTES', framePacotes1.Pacotes[I].Caption, False);
  finally
    ArqIni.Free;
  end;
end;

// gravar as configurações efetuadas pelo usuário
procedure TfrmPrincipal.GravarConfiguracoesEmArquivoIni;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    ArqIni.WriteString('CONFIG', 'DiretorioInstalacao', edtDirDestino.Text);
    ArqIni.WriteInteger('CONFIG','DestinoDLL', rdgDLL.ItemIndex);
    ArqIni.WriteBool('CONFIG','C++Builder', ckbBCB.Checked);
    ArqIni.WriteBool('CONFIG','DexarSomenteLib', chkDeixarSomenteLIB.Checked);
    ArqIni.WriteBool('CONFIG','RemoveOpenSSL', ckbRemoveOpenSSL.Checked);
    ArqIni.WriteBool('CONFIG','RemoveCapicom', ckbRemoveCapicom.Checked);
    ArqIni.WriteBool('CONFIG','RemoveXmlSec', ckbRemoveXMLSec.Checked);
    ArqIni.WriteBool('CONFIG','CargaDllTardia', ckbCargaDllTardia.Checked);
    ArqIni.WriteBool('CONFIG','RemoverCastWarnings', ckbRemoverCastWarnings.Checked);

    for I := 0 to framePacotes1.Pacotes.Count - 1 do
      ArqIni.WriteBool('PACOTES', framePacotes1.Pacotes[I].Caption, framePacotes1.Pacotes[I].Checked);
  finally
    ArqIni.Free;
  end;
end;

procedure TfrmPrincipal.IniciaNovaInstalacao(const MaximoPassosProgresso: Integer; const
    NomeCaminhoArquivoLog: string; const Cabecalho: string);
begin
  // limpar o log
  lstMsgInstalacao.Clear;
  // setar barra de progresso
  pgbInstalacao.Position := 0;

  pgbInstalacao.Max := MaximoPassosProgresso;

  // mostrar ao usuário as informações de compilação
  FUltimoArquivoLog := NomeCaminhoArquivoLog;
  lbInfo.Clear;
  lbInfo.Items.Text := Cabecalho;
end;

procedure TfrmPrincipal.MontaListaIDEsSuportadas;
var
  iFor: Integer;
//  tcpt: TCompileTargetList;
begin

//  tcpt := TCompileTargetList.Create;
//
//  for iFor := 0 to tcpt.Count - 1 do
//  begin
//
//    clbDelphiVersion.Items.Add(tcpt.Items[iFor].DisplayName);
//    //Desabilitar versões não suportadas
//    clbDelphiVersion.ItemEnabled[iFor] := tcpt.Items[iFor].IsValid and (tcpt.Items[iFor].Version >= 7);
//  end;

  // popular o combobox de versões do delphi instaladas na máquina
  for iFor := 0 to FoACBr.Count - 1 do
  begin
    clbDelphiVersion.Items.Add(VersionNumberToNome(FoACBr.Installations[iFor].VersionNumberStr));
    //Desabilitar versões não suportadas
    clbDelphiVersion.ItemEnabled[iFor] := (not MatchText(FoACBr.Installations[iFor].VersionNumberStr, ['d3', 'd4', 'd5', 'd6']));
  end;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  FoACBr := TJclBorRADToolInstallations.Create;
  FUltimoArquivoLog := '';

  MontaListaIDEsSuportadas;

  LerConfiguracoes;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FoACBr.Free;
end;

procedure TfrmPrincipal.Logar(const AString: String);
begin
  lstMsgInstalacao.Items.Add(AString);
  lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
  Application.ProcessMessages;
end;

function TfrmPrincipal.ProcedeInstalacao: Boolean;
var
  ACBrInstaladorAux: TACBrInstallComponentes;
  ListaPacotes: TPacotes;
  ListaVersoesInstalacao: TList<Integer>;
  i: Integer;
begin
  ListaPacotes := framePacotes1.Pacotes;

  ListaVersoesInstalacao := TList<Integer>.Create;

  for i := 0 to FoACBr.Count - 1 do
  begin
    // só instala as versão marcadas para instalar.
    if (clbDelphiVersion.Checked[i]) then
    begin
      ListaVersoesInstalacao.Add(i)
    end;
  end;

  ACBrInstaladorAux := TACBrInstallComponentes.Create(Application);
  try
    ACBrInstaladorAux.OnIniciaNovaInstalacao := IniciaNovaInstalacao;
    ACBrInstaladorAux.OnProgresso            := IncrementaBarraProgresso;
    ACBrInstaladorAux.OnInformaSituacao      := Logar;

    ACBrInstaladorAux.Opcoes.LimparArquivosACBrAntigos := ckbRemoverArquivosAntigos.Checked;
    ACBrInstaladorAux.Opcoes.DeixarSomentePastasLib    := chkDeixarSomenteLIB.Checked;
    ACBrInstaladorAux.Opcoes.DeveInstalarCapicom       := not ckbRemoveCapicom.Checked;
    ACBrInstaladorAux.Opcoes.DeveInstalarOpenSSL       := not ckbRemoveOpenSSL.Checked;
    ACBrInstaladorAux.Opcoes.DeveCopiarOutrasDLLs      := ckbCopiarTodasDll.Checked;
    ACBrInstaladorAux.Opcoes.DeveInstalarXMLSec        := not ckbRemoveXMLSec.Checked;
    ACBrInstaladorAux.Opcoes.UsarCargaTardiaDLL        := ckbCargaDllTardia.Checked;
    ACBrInstaladorAux.Opcoes.RemoverStringCastWarnings := ckbRemoverCastWarnings.Checked;
    ACBrInstaladorAux.Opcoes.UsarCpp                   := ckbBCB.Checked;
    ACBrInstaladorAux.Opcoes.UsarUsarArquivoConfig     := ckbUsarArquivoConfig.Checked;
    case rdgdll.ItemIndex of
      0 : ACBrInstaladorAux.Opcoes.sDestinoDLLs := tdSystem;
      1 : ACBrInstaladorAux.Opcoes.sDestinoDLLs := tdDelphi;
      2 : ACBrInstaladorAux.Opcoes.sDestinoDLLs := tdNone;
    else
      ACBrInstaladorAux.Opcoes.sDestinoDLLs     := tdNone;
    end;
    ACBrInstaladorAux.Opcoes.DiretorioRaizACBr := IncludeTrailingPathDelimiter(edtDirDestino.Text);


    Result := ACBrInstaladorAux.Instalar(FoACBr, ListaPacotes, ListaVersoesInstalacao);
  finally
    ACBrInstaladorAux.Free;
  end;

  if Result then
  begin
    Application.MessageBox(PWideChar('Pacotes compilados e instalados com sucesso! ' + sLineBreak +
                                     'Clique em "Próximo" para finalizar a instalação.'),
                           'Instalação', MB_ICONINFORMATION + MB_OK);
  end
  else
  begin
    if Application.MessageBox(PWideChar('Ocorreram erros durante o processo de instalação, ' + sLineBreak +
                                        'para maiores informações verifique o arquivo de log gerado.' +
                                        sLineBreak + sLineBreak +
                                        'Deseja visualizar o arquivo de log gerado?'),
                              'Instalação', MB_ICONQUESTION + MB_YESNO) = ID_YES then
    begin
      AbrirArquivoLogAtual;
    end;
  end;

end;

procedure TfrmPrincipal.AbrirArquivoLogAtual;
begin
  if FUltimoArquivoLog = '' then
  begin
    Exit;
  end;
  ShellExecute(Handle, 'open', PWideChar(FUltimoArquivoLog), '', '', 1);
end;

procedure TfrmPrincipal.IncrementaBarraProgresso;
begin
  pgbInstalacao.Position := pgbInstalacao.Position + 1;
  Application.ProcessMessages;
end;

// botão de compilação e instalação dos pacotes selecionados no treeview
procedure TfrmPrincipal.btnInstalarACBrClick(Sender: TObject);
var
  Instalou: Boolean;
begin
  Instalou := False;
  btnInstalarACBr.Enabled := False;
  btnVisualizarLogCompilacao.Enabled := False;
  wizPgInstalacao.EnableButton(bkBack, False);
  wizPgInstalacao.EnableButton(bkNext, False);
  wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), False);
  try
    Instalou := ProcedeInstalacao;
  finally
    btnInstalarACBr.Enabled := True;
    btnVisualizarLogCompilacao.Enabled := True;
    wizPgInstalacao.EnableButton(bkBack, True);
    wizPgInstalacao.EnableButton(bkNext, Instalou);
    wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), True);
  end;
end;

// chama a caixa de dialogo para selecionar o diretório de instalação
// seria bom que a caixa fosse aquele que possui o botão de criar pasta
procedure TfrmPrincipal.btnSelecDirInstallClick(Sender: TObject);
var
  Dir: String;
begin
  if SelectDirectory('Selecione o diretório de instalação', '', Dir, [sdNewFolder, sdNewUI, sdValidateDir]) then
    edtDirDestino.Text := Dir;
end;

// quando trocar a versão verificar se libera ou não o combo
// da plataforma de compilação
procedure TfrmPrincipal.clbDelphiVersionClick(Sender: TObject);
begin
  if clbDelphiVersion.ItemIndex < 0 then
  begin
    Exit
  end;

  if MatchText(FoACBr.Installations[clbDelphiVersion.ItemIndex].VersionNumberStr, ['d7','d9','d10','d11']) then
  begin
    Application.MessageBox(
      'Atenção: Embora o ACBr continue suportando versões anteriores do Delphi, incentivamos que você atualize o quanto antes para versões mais recentes do Delphi ou considere migrar para o Lazarus.',
      'Erro.',
      MB_OK + MB_ICONWARNING
    );
  end;

  // C++ Builder a partir do D2006, versões anteriores tem IDE independentes.
  ckbBCB.Enabled := MatchText(FoACBr.Installations[clbDelphiVersion.ItemIndex].VersionNumberStr, ['d10','d11','d12','d14','d15','d16','d17','d18','d19','d20','d21','d22','d23','d24','d25','d26']);
  if not ckbBCB.Enabled then
     ckbBCB.Checked := False;
end;

// abrir o endereço do ACBrSAC quando clicar na propaganda
procedure TfrmPrincipal.imgPropaganda1Click(Sender: TObject);
begin
  // ir para o endereço do ACBrSAC
  ShellExecute(Handle, 'open', PWideChar(lblUrlACBrSac1.Caption), '', '', 1);
end;

// quando clicar em alguma das urls chamar o link mostrado no caption
procedure TfrmPrincipal.URLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(TLabel(Sender).Caption), '', '', 1);
end;

procedure TfrmPrincipal.wizPgInicioNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  // Verificar se o delphi está aberto
  {$IFNDEF DEBUG}
  if FoACBr.AnyInstanceRunning then
  begin
    Stop := True;
    Application.MessageBox(
      'Feche a IDE do delphi antes de continuar.',
      PWideChar(Application.Title),
      MB_ICONERROR + MB_OK
    );
  end;
  {$ENDIF}
end;

procedure TfrmPrincipal.wizPgInstalacaoEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
begin
//  // para 64 bit somente compilar
//  if tPlatform = bpWin32 then // Win32
//    btnInstalarACBr.Caption := 'Instalar'
//  else // win64
//    btnInstalarACBr.Caption := 'Compilar';
//
  lbInfo.Clear;
  lbInfo.Items.Add('Pressione o botão "'+ btnInstalarACBr.Caption +'" para prosseguir.');
  btnVisualizarLogCompilacao.Enabled := False;
end;

procedure TfrmPrincipal.wizPgInstalacaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  if (lstMsgInstalacao.Count <= 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Clique no botão instalar antes de continuar.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

//  Não deve ser permitido clicar em Next caso tenha havido erros.
//  Assim o código abaixo é desnecessário.
//  if (FCountErros > 0) then
//  begin
//    Stop := True;
//    Application.MessageBox(
//      'Ocorreram erros durante a compilação e instalação dos pacotes, verifique.',
//      'Erro.',
//      MB_OK + MB_ICONERROR
//    );
//  end;
end;

procedure TfrmPrincipal.wizPgConfiguracaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  // verificar se foi informado o diretório
  if Trim(edtDirDestino.Text) = EmptyStr then
  begin
    Stop := True;
    edtDirDestino.SetFocus;
    Application.MessageBox(
      'Diretório de instalação não foi informado.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  if not ckbRemoveXMLSec.Checked then
  begin
    if MessageDlg('Usar XMLSec não é recomendado. Sugerimos que marque a opção "'+
                  ckbRemoveXMLSec.Caption + '" antes de continuar.'+ sLineBreak +
                  'Deseja continuar assim mesmo?', mtConfirmation, mbYesNo, 0, mbNo) <> mrYes  then
    begin
      Stop := True;
    end;
  end;

//  //Exibir mensagem abaixo apenas se não for o Delphi 7..
//  if not ckbRemoverCastWarnings.Checked then
//  begin
//    if MessageDlg('Se não estiver resolvendo os Warnings com strings sugerimos marcar a opção "'+
//                  ckbRemoverCastWarnings.Caption + '" antes de continuar.'+ sLineBreak +
//                  'Deseja continuar assim mesmo?', mtConfirmation, mbYesNo, 0, mbNo) <> mrYes  then
//    begin
//      Stop := True;
//    end;
//  end;
//
  if (ckbRemoverArquivosAntigos.Checked) and
     (MessageDlg('Você optou por limpar arquivos antigos do ACBr do seu computador. Essa ação pode demorar vários minutos.' + sLineBreak +
                 'Deseja realmente continuar?', mtConfirmation, mbYesNo, 0, mbNo) <> mrYes) then
  begin
    Stop := True;
    ckbRemoverArquivosAntigos.Checked := False;
  end;

  if not ckbCopiarTodasDll.Checked then
  begin
    if MessageDlg('Não foi marcado a opção para copiar as DLLs. Você terá que copiar manualmente. ' + sLineBreak +
                  'Deseja continuar assim mesmo?', mtConfirmation, mbYesNo, 0, mbNo) <> mrYes  then
    begin
      Stop := True;
    end;
  end;

  ValidarSeExistemPacotesNasPastas(Stop, IncludeTrailingPathDelimiter(edtDirDestino.Text), framePacotes1.Pacotes);
end;

procedure TfrmPrincipal.btnVisualizarLogCompilacaoClick(Sender: TObject);
begin
  AbrirArquivoLogAtual;
end;

procedure TfrmPrincipal.wizPgPacotesNextButtonClick(Sender: TObject; var Stop: Boolean);
begin
  // Gravar as configurações em um .ini para utilizar depois
  GravarConfiguracoesEmArquivoIni;
end;

procedure TfrmPrincipal.wizPgSelectIDEsNextButtonClick(Sender: TObject; var Stop: Boolean);
var
  iFor: Integer;
  bChk: Boolean;
begin
  bChk := False;
  for iFor := 0 to clbDelphiVersion.Count -1 do
  begin
    if clbDelphiVersion.Checked[iFor] then
    begin
      bChk := True;
      Break;
    end;
  end;

  if not bChk then
  begin
    Stop := True;
    clbDelphiVersion.SetFocus;
    Application.MessageBox(
      'Para continuar escolha a versão do Delphi para a qual deseja instalar o ACBr.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

end;

procedure TfrmPrincipal.wizPrincipalCancelButtonClick(Sender: TObject);
begin
  if Application.MessageBox(
    'Deseja realmente cancelar a instalação?',
    'Fechar',
    MB_ICONQUESTION + MB_YESNO
  ) = ID_YES then
  begin
    Self.Close;
  end;
end;

procedure TfrmPrincipal.wizPrincipalFinishButtonClick(Sender: TObject);
begin
  Self.Close;
end;

end.
