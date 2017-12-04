unit uExemploReinf;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, pcnConversaoReinf,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ACBrReinf, ACBrReinfWebServices,
  ACBrReinfEventos, ACBrBase, ACBrDFe;

type
  TForm2 = class(TForm)
    Label5: TLabel;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    btnGerar: TButton;
    edProtocolo: TEdit;
    chkClear: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    chk1000: TCheckBox;
    chk2010: TCheckBox;
    chk2020: TCheckBox;
    chk2098: TCheckBox;
    chk1070: TCheckBox;
    rdgOperacao: TRadioGroup;
    rdgGrupo: TRadioGroup;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    edRecibo: TEdit;
    cbEvento: TComboBox;
    TabSheet2: TTabSheet;
    mmoRet: TMemo;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    TabSheet4: TTabSheet;
    Memo2: TMemo;
    chk2099: TCheckBox;
    chk9000: TCheckBox;
    ChkRetificadora: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Button1: TButton;
    procedure btnGerarClick(Sender: TObject);
  private
    FACBrReinf: TACBrReinf;
    procedure PreencherXMLEventos;
    procedure LimparDocsPasta;
    procedure Configurar(AACBrReinf: TACBrReinf);
    function GetTipoOperacao: TTypeOperacao;
    {Eventos}
    procedure GerarReinf1000;
    procedure GerarReinf1070;
    procedure GerarReinf2010;
    procedure GerarReinf2020;
    procedure GerarReinf2099;
    procedure GerarReinf2098;
    procedure GerarReinf9000;
  public
    procedure DepoisDeEnviar(const Axml: string);
    procedure AntesDeEnviar(const Axml: string);
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

uses ACBrDFeSSL, pcnConversao, ShellAPI, ACBrReinfClasses, ACBrReinfR1070, ACBrReinfR1000,
  ACBrReinfR2010, ACBrReinfR2020, ACBrReinfR2099, ACBrReinfR2098, ACBrReinfR9000;

procedure TForm2.AntesDeEnviar(const Axml: string);
begin
  Memo1.Lines.Text := Axml;
end;

procedure TForm2.btnGerarClick(Sender: TObject);
var
  Evento: TEvento;
  Ocorrencia: TOcorrencia;
begin
  mmoRet.Clear;
  FACBrReinf := TACBrReinf.Create(nil);
  FACBrReinf.OnBeforeEnviar := AntesDeEnviar;
  FACBrReinf.OnAfterEnviar := DepoisDeEnviar;
  try
    Configurar(FACBrReinf);

    PreencherXMLEventos;

    if FACBrReinf.Enviar then
    begin
      mmoRet.Lines.Add('ideTransmissor: '+ FACBrReinf.WebServices.RetEventos.IdeTransmissor.IdTransmissor);
      mmoRet.Lines.Add('cdStatus: '+ IntToStr(FACBrReinf.WebServices.RetEventos.Status.cdStatus));
      mmoRet.Lines.Add('retornoEventos');
      for Evento in FACBrReinf.WebServices.RetEventos.Eventos do
      begin
        mmoRet.Lines.Add('Evento Id: ' + Evento.Id);
        mmoRet.Lines.Add('*ideContrib ');
        mmoRet.Lines.Add(' ideContrib: ' + Inttostr(ord(Evento.ideContrib.TpInsc)));
        mmoRet.Lines.Add(' NrInsc: ' + Evento.ideContrib.NrInsc);
        mmoRet.Lines.Add('*dadosRecepcaoEvento ');
        mmoRet.Lines.Add(' dhProcessamento ' + Datetostr(Evento.dadosRecepcaoEvento.dhProcessamento));
        mmoRet.Lines.Add(' tipoEvento ' + Evento.dadosRecepcaoEvento.tipoEvento);
        mmoRet.Lines.Add(' IdEvento ' + Evento.dadosRecepcaoEvento.IDEvento);
        mmoRet.Lines.Add(' Hash ' + Evento.dadosRecepcaoEvento.Hash);
        edProtocolo.Text := Evento.dadosRecepcaoEvento.Hash;
        mmoRet.Lines.Add('*Status ');
        mmoRet.Lines.Add(' cdRetorno ' + Inttostr(Evento.Status.cdRetorno));
        mmoRet.Lines.Add(' descRetorno ' + Evento.Status.descRetorno);
        if Evento.Status.cdRetorno = 0 then {Sucesso}
        begin
          mmoRet.Lines.Add('*dadosReciboEntrega');
          mmoRet.Lines.Add(' numeroRecibo ' + Evento.dadosReciboEntrega.numeroRecibo);
        end;
        mmoRet.Lines.Add(' **Ocorrencias');
        for Ocorrencia in Evento.Status.Ocorrencias do
        begin
          mmoRet.Lines.Add('   codigo: ' + Ocorrencia.codigo);
          mmoRet.Lines.Add('   descricao: ' + Ocorrencia.descricao);
          mmoRet.Lines.Add('   tipo: ' + Inttostr(Ocorrencia.tipo));
          mmoRet.Lines.Add('   localizacaoErroAviso: ' + Ocorrencia.localizacaoErroAviso);
        end;
      end;
    end
    else
      ShowMessage('Falha');
  finally
    FACBrReinf.Free;
  end;
end;

procedure TForm2.Configurar(AACBrReinf: TACBrReinf);
begin
  // Configuracao Geral
  AACBrReinf.Configuracoes.Arquivos.PathSchemas := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'Schemas';
  AACBrReinf.Configuracoes.Arquivos.Salvar := False;
  AACBrReinf.Configuracoes.Arquivos.SepararPorCNPJ := True;
  AACBrReinf.Configuracoes.Geral.Salvar := True;
  AACBrReinf.Configuracoes.WebServices.Salvar  := True;
  AACBrReinf.Configuracoes.Geral.FormaEmissao := teNormal;
  AACBrReinf.Configuracoes.WebServices.Ambiente := taHomologacao;
  AACBrReinf.Configuracoes.WebServices.Salvar := True;
  // Configuracao WebService
  AACBrReinf.Configuracoes.WebServices.UF := 'CE';
  AACBrReinf.Configuracoes.Certificados.VerificarValidade := True;
  AACBrReinf.Configuracoes.WebServices.AguardarConsultaRet      := 5000; // tempo padrão que vai aguardar para consultar após enviar a NF-e
  AACBrReinf.Configuracoes.WebServices.IntervaloTentativas      := 3000; // Intervalo entre as tentativas de envio
  AACBrReinf.Configuracoes.WebServices.Tentativas               := 10;   // quantidade de tentativas de envio
  AACBrReinf.Configuracoes.WebServices.AjustaAguardaConsultaRet := True; // ajustar "AguardarConsultaRet" com o valor retornado pelo webservice
  // Configuracao Certificados
  AACBrReinf.Configuracoes.Geral.SSLLib := libOpenSSL;
  AACBrReinf.Configuracoes.Geral.SSLHttpLib := httpWinHttp;

  AACBrReinf.Configuracoes.Certificados.ArquivoPFX := 'C:\Certificados\CERT.pfx';
  AACBrReinf.Configuracoes.Certificados.NumeroSerie := '23bce65ca331e63aebe165850790c9a6';
  AACBrReinf.Configuracoes.Certificados.Senha := '789456123';

  {Identificação}
 // AACBrReinf.IdEmpregador :=  '02191905000111';
 // AACBrReinf.IdTransmissor := '02191905000111';

  {IdeEvento}
  FACBrReinf.IdeEvento.TpAmb := taProducaoRestritaDadosReais;
  FACBrReinf.IdeEvento.ProcEmi := peAplicEmpregador;
  FACBrReinf.IdeEvento.VerProc := '1.0';
  {IdeEvento}
  FACBrReinf.ideContri.TpInsc := tiCNPJ;
  FACBrReinf.ideContri.NrInsc := '02191905000111';
end;

procedure TForm2.DepoisDeEnviar(const Axml: string);
begin
  Memo2.Lines.Text := Axml;
end;

procedure TForm2.LimparDocsPasta;
var
  path: string;
  FileOp: TSHFileOpStruct;
begin
  try
    path := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'Docs';
    FillChar(FileOp, SizeOf(FileOp), 0);
    FileOp.wFunc := FO_DELETE;
    FileOp.pFrom := PChar(path+#0);//double zero-terminated
    FileOp.fFlags := FOF_SILENT or FOF_NOERRORUI or FOF_NOCONFIRMATION;
    SHFileOperation(FileOp);
    ForceDirectories(path);
  except
  end;
end;

procedure TForm2.PreencherXMLEventos;
begin
  if chk1000.Checked then
    GerarReinf1000;

  if chk1070.Checked then
    GerarReinf1070;

  if chk2010.Checked then
    GerarReinf2010;

  if chk2020.Checked then
    GerarReinf2020;

  if chk2098.Checked then
    GerarReinf2098;

  if chk2099.Checked then
    GerarReinf2099;

  if chk9000.Checked then
    GerarReinf9000;
end;


procedure TForm2.GerarReinf1000;
begin
  with FACBrReinf.Eventos.AddR1000 do
  begin
    TipoOperacao := GetTipoOperacao;
    infoContri.IdePeriodo.IniValid := '2017-01';
    // evtInfoEmpregador.InfoEmpregador.IdePeriodo.FimValid := '2099-12';
    infoContri.InfoCadastro.ClassTrib := '99';

    infoContri.InfoCadastro.indEscrituracao := tpNao;;
    infoContri.InfoCadastro.indDesoneracao := tpNao;
    infoContri.InfoCadastro.indAcordoIsenMulta := tpNao;

    infoContri.InfoCadastro.Contato.NmCtt := 'Leivio Fonteneke';
    infoContri.InfoCadastro.Contato.CpfCtt := '96305312320';
    infoContri.InfoCadastro.Contato.FoneFixo := '8534444563';
    infoContri.InfoCadastro.Contato.FoneCel := '8584444563';
    infoContri.InfoCadastro.Contato.email := 'leivio@yahoo.com.br';

    with infoContri.InfoCadastro.SoftwareHouse do
    begin
      CnpjSoftHouse := '02191905000111';
      NmRazao := 'Datafocus Sistemas';
      NmCont := 'Contato';
      Telefone := '8534335856';
      email := 'teste@teste.com';
    end;

    infoContri.InfoCadastro.indSitPJ := spNormal;
  end;
end;

procedure TForm2.GerarReinf1070;
var
  infoSusp: TinfoSusp;
begin
  with FACBrReinf.Eventos.AddR1070 do
  begin
    TipoOperacao := GetTipoOperacao;
    InfoProcesso.IdePeriodo.IniValid := '2017-01';
    // evtInfoEmpregador.InfoEmpregador.IdePeriodo.FimValid := '2099-12';
    InfoProcesso.IdeProcesso.tpProc := tpJudicial;
    InfoProcesso.IdeProcesso.nrProc := '01377929720138060001';
    InfoProcesso.IdeProcesso.DadosProcJud.UfVara     := 'CE';
    InfoProcesso.IdeProcesso.DadosProcJud.codMunic   := 2304400;
    InfoProcesso.IdeProcesso.DadosProcJud.IdVara     := '03';
    infoSusp := TinfoSusp(InfoProcesso.IdeProcesso.infoSusps.Items[InfoProcesso.IdeProcesso.infoSusps.Add(TinfoSusp.Create)]);
    //infoSusp.codSusp := '1';
    infoSusp.indSusp := siDecisaoDefinitivaAFavorDoContribuinte;
    infoSusp.dtDecisao := StrtoDate('01/01/2017');
    infoSusp.indDeposito := tpNao;
  end;
end;

procedure TForm2.GerarReinf2010;
var
  R2010: TR2010;
begin
  R2010 := FACBrReinf.Eventos.AddR2010;
  with R2010 do
  begin
    R2010.indRetif := trOriginal;

    if ChkRetificadora.Checked then
      R2010.indRetif := trRetificacao;

    if R2010.indRetif = trRetificacao then
      R2010.nrRecibo := edRecibo.Text;
    R2010.perApur := '2017-08';

    R2010.infoServTom.IdePeriodo.IniValid := '2017-01';

    R2010.infoServTom.ideEstabObra.tpInscEstab := tiCNPJ;
    R2010.infoServTom.ideEstabObra.nrInscEstab := '02191905000111';
    R2010.infoServTom.ideEstabObra.indObra := ioNaoeObraDeConstrucaoCivil;

    with R2010.infoServTom.ideEstabObra.idePrestServs.Items[R2010.infoServTom.ideEstabObra.idePrestServs.Add(TidePrestServ.Create)] do
    begin
      cnpjPrestador := '09473513000194';
      vlrTotalBruto := 100;
      vlrTotalBaseRet := 100;
      vlrTotalRetPrinc := 11;
  //  vlrTotalRetAdic := 0;
  //  vlrTotalNRetPrinc := 100;
      codAnaCont := '001';
      indCPRB := icNaoContribuintePrevidenciariaReceitaBruta;
      with nfss.Items[nfss.Add(Tnfs.Create)] do
      begin
        serie := '00001';
        numDocto  := '0000000001';
        dtEmissaoNF := Now;
        vlrBruto  := 100;
        obs := '';
        with infoTpServs.Items[infoTpServs.Add(TinfoTpServ.Create)] do
        begin
          tpServico := '06'; {Tabela 06}
          //codAtivEcon := '00000025';
          vlrMatEquip := 0;
          vlrDedAlim := 0;
          vlrDedTrans := 0;
          vlrBaseRet := 100;
          vlrRetencao := 11;
          //vlrRetSub := 0;
        end;
      end;
      {
      with infoProcRetPrs.Items[infoProcRetPrs.Add(TinfoProcRetPr.Create)] do
      begin
        tpProcRetPrinc := tprAdministrativoTomador;
        nrProcRetPrinc := '1122112';
        codSuspPrinc := 001;
        valorPrinc := 100.00;
      end;
      }
    end;
  end;
end;

procedure TForm2.GerarReinf2020;
var
  R2020: TR2020;
begin
  R2020 := FACBrReinf.Eventos.AddR2020;
  with R2020 do
  begin
    R2020.indRetif := trOriginal;

    if ChkRetificadora.Checked then
      R2020.indRetif := trRetificacao;

    if R2020.indRetif = trRetificacao then
      R2020.nrRecibo := edRecibo.Text;
    R2020.perApur := '2017-08';

    R2020.infoServPrest.IdePeriodo.IniValid := '2017-01';

     R2020.infoServPrest.ideEstabPrest.tpInscEstabPrest := ord(tiCNPJ); {valor somente leitura -> Valor fixo 1}
    R2020.infoServPrest.ideEstabPrest.nrInscEstabPrest := '02191905000111';

    with R2020.infoServPrest.ideEstabPrest.ideTomadors.Items[R2020.infoServPrest.ideEstabPrest.ideTomadors.Add(TideTomador.Create)] do
    begin
      tpInscTomador := tiCNPJ; {Não preencher fixo}
      nrInscTomador := '09473513000194';
      vlrTotalBruto := 100;
      vlrTotalBaseRet := 100;
      vlrTotalRetPrinc := 11;
      codAnaCont := '001';
      with nfss.Items[nfss.Add(Tnfs.Create)] do
      begin
        serie := '00001';
        numDocto  := '0000000001';
        dtEmissaoNF := Now;
        vlrBruto  := 100;
        obs := '';
        with infoTpServs.Items[infoTpServs.Add(TinfoTpServ.Create)] do
        begin
          tpServico := '06'; {Tabela 06}
          codAtivEcon := '00000025';
          vlrMatEquip := 0;
          vlrDedAlim := 0;
          vlrDedTrans := 0;
          vlrBaseRet := 100;
          vlrRetencao := 11;
          //vlrRetSub := 0;
        end;
      end;
      {
      with infoProcRetPrs.Items[infoProcRetPrs.Add(TinfoProcRetPr.Create)] do
      begin
        tpProcRetPrinc := tprAdministrativoTomador;
        nrProcRetPrinc := '1122112';
        codSuspPrinc := 001;
        valorPrinc := 100.00;
      end;
      }
    end;
  end;
end;

procedure TForm2.GerarReinf2098;
var
  R2098: TR2098;
begin
  R2098 := FACBrReinf.Eventos.AddR2098;
  with R2098 do
  begin
    R2098.perApur := '2017-08';
  end;
end;

procedure TForm2.GerarReinf2099;
var
  R2099: TR2099;
begin
  R2099 := FACBrReinf.Eventos.AddR2099;
  with R2099 do
  begin
    perApur := '2017-08';
    with R2099.ideRespInf do
    begin
      nmResp := 'Leivio Fontenele';
      cpfResp := '96305312320';
      telefone := '8591854655';
      email := 'leviio@yahoo.com.br'
    end;

    with R2099.infoFech do
    begin
      evtServTm := tpSim;
      evtServPr := tpSim;
      evtAssDespRec := tpNao;
      evtAssDespRep := tpNao;
      evtComProd := tpSim;
      evtComProd := tpSim;
      evtCPRB := tpNao;
      evtPgtos := tpNao;
      //compSemMovto := '2017-01'; {Somente preenchido se os outros valores forem tbNao}
    end;
  end;
end;

procedure TForm2.GerarReinf9000;
var
  R9000: TR9000;
begin
  R9000 := FACBrReinf.Eventos.AddR9000;
  with R9000 do
  begin
    infoExclusao.tpEvento := cbEvento.Items.Strings[cbEvento.ItemIndex];
    infoExclusao.nrRecEvt := Trim(edRecibo.Text);
    infoExclusao.perApur := '2017-08';
  end;

end;

function TForm2.GetTipoOperacao: TTypeOperacao;
begin
  case rdgOperacao.ItemIndex of
    1: Result := toAlteracao;
    2: Result := toExclusao;
  else
    Result := toInclusao;
  end;
end;

end.

{

9.2.Eventos
Inicialmente, o ambiente de Produção Restrita será disponibilizado contendo os
eventos abaixo que foram implementados de acordo com a versão 1.1 do leiaute e da versão
1_01_01 dos schemas XML:
1. R-1000 - Informações do Empregador/Contribuinte
2. R-1070 - Tabela de Processos Administrativos/Judiciais
3. R-2010 – Retenção Contribuição Previdenciária - Serviços Tomados
4. R-2020 – Retenção Contribuição Previdenciária - Serviços Prestados
  5. R-2030 – Recursos Recebidos por Associação Desportiva  (Não vamos fazer)
  6. R-2040 – Recursos Repassados para Associação Desportiva (Não Vamos Fazer)
7. R-2098 – Reabertura dos Eventos Periódicos
8. R-2099 – Fechamento dos Eventos Periódicos
9. R-9000 – Exclusão de Eventos
As datas para disponibilização de versões futuras da EFD-REINF nos ambientes de
Produção Restrita e Produção serão divulgadas oportunamente.

}
