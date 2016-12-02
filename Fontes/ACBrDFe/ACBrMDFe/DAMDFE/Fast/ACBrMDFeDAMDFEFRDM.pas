{ ******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                  }
{ Biblioteca multiplataforma de componentes Delphi                              }
{                                                                               }
{ Você pode obter a última versão desse arquivo na pagina do Projeto ACBr       }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr            }
{                                                                               }
{                                                                               }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la   }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM     }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto  }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/lgpl-license.php                           }
{                                                                               }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br   }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410                                }
{                                                                               }
{ ******************************************************************************}
{ ******************************************************************************
  |* Historico
  |*
  |* 18/10/2013: Jeanny Paiva Lopes
  |*  - Inicio do desenvolvimento DAMDFE FastReport
  ****************************************************************************** }

{$I ACBr.inc}

unit ACBrMDFeDAMDFEFRDM;

interface

uses
  SysUtils, Classes, frxBarcode, frxClass, frxExportPDF, frxDBSet, DB, DBClient,
  ACBrMDFeDAMDFeClass, pmdfeMDFe, ACBrMDFe, pcnConversao, Dialogs,
  pmdfeEnvEventoMDFe, StrUtils;

type
  TDMACBrMDFeDAMDFEFR = class(TDataModule)
    cdsIdentificacao: TClientDataSet;
    frxIdentificacao: TfrxDBDataset;
    cdsEmitente: TClientDataSet;
    frxEmitente: TfrxDBDataset;
    cdsModalRodo: TClientDataSet;
    frxModalRodo: TfrxDBDataset;
    frxModalAereo: TfrxDBDataset;
    CDSModalAereo: TClientDataSet;
    CDSModalAqua: TClientDataSet;
    frxModalAqua: TfrxDBDataset;
    frxModalFerrov: TfrxDBDataset;
    CDSModalFerrov: TClientDataSet;
    cdsParametros: TClientDataSet;
    frxParametros: TfrxDBDataset;
    CDSModalFerrovVagoes: TClientDataSet;
    frxModalFerrovVagoes: TfrxDBDataset;
    CDSDocumentos: TClientDataSet;
    frxDocumentos: TfrxDBDataset;
    cdsEventos: TClientDataSet;
    frxEventos: TfrxDBDataset;
    cdsMunCarrega: TClientDataSet;
    frxMunCarrega: TfrxDBDataset;
    cdsPercurso: TClientDataSet;
    frxPercurso: TfrxDBDataset;
    frxTermCarrega: TfrxDBDataset;
    frxTermDescarrega: TfrxDBDataset;
    frxEmbarcaComboio: TfrxDBDataset;
    frxInfUnidCargaVazia: TfrxDBDataset;
    cdsTermCarrega: TClientDataSet;
    cdsTermDescarrega: TClientDataSet;
    cdsEmbarcaComboio: TClientDataSet;
    cdsInfUnidCargaVazia: TClientDataSet;
    constructor Create(AOwner: TComponent); override;
    procedure frxReportGetValue(const VarName: string; var Value: Variant);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FDAMDFEClassOwner: TACBrMDFeDAMDFeClass;
    FMDFe            : TMDFe;
    FEvento          : TEventoMDFe;
    procedure CarregaIdentificacao;
    procedure CarregaParametros;
    procedure CarregaEmitente;
    procedure CarregaDocumentos;
    procedure CarregaModal;
    procedure CarregaModalRodoviario;
    procedure CarregaModalAereo;
    procedure CarregaModalAquaviario;

    procedure CarregaTermCarreg;
    procedure CarregaTermDescarreg;
    procedure CarregaEmbarcacaoComboio;
    procedure CarregaInfUnidCargaVazia;

    procedure CarregaModalFerroviario;
    procedure CarregaMunCarrega;
    procedure CarregaPercurso;
  public
    frxPDFExport: TfrxPDFExport;
    frxReport: TfrxReport;
    frxBarCodeObject: TfrxBarCodeObject;

    property MDFe            : TMDFe read FMDFe write FMDFe;
    property Evento          : TEventoMDFe read FEvento write FEvento;
    property DAMDFEClassOwner: TACBrMDFeDAMDFeClass read FDAMDFEClassOwner;
    procedure CarregaDados;
    procedure CarregaDadosEventos;
    procedure SetDataSetsToFrxReport;
  end;

function CollateBr(Str: string): string;

var
  DMACBrMDFeDAMDFEFR: TDMACBrMDFeDAMDFEFR;

implementation

uses
  ACBrDFeUtil, pmdfeConversaoMDFe, ACBrValidador, ACBrUtil;

type
  TSplitResult = array of string;

{$R *.dfm}


procedure TDMACBrMDFeDAMDFEFR.CarregaDados;
begin
  CarregaParametros;
  CarregaIdentificacao;
  CarregaMunCarrega;
  CarregaPercurso;
  CarregaEmitente;
  CarregaDocumentos;
  CarregaModal;

  CarregaTermCarreg;
  CarregaTermDescarreg;
  CarregaEmbarcacaoComboio;
  CarregaInfUnidCargaVazia;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaModal;
begin

  // {$REGION 'Modal Rodoviario'}
  with cdsModalRodo, FieldDefs do
  begin
    Close;
    Clear;
    Add('RNTRC', ftString, 8);
    Add('CIOT', ftString, 12);
    // Vale Pedagio
    Add('CNPJForn', ftMemo);
    Add('CNPJPg', ftMemo);
    Add('nCompra', ftMemo);
    // Veiculos
    Add('placa', ftMemo);
    Add('RNTRCProp', ftMemo);
    // Condutor
    Add('xNome', ftMemo);
    Add('CPF', ftMemo);
    CreateDataSet;
  end;
  // {$ENDREGION}

  with CDSModalAereo, FieldDefs do
  begin
    Close;
    Clear;
    // Aereo
    Add('nac', ftInteger);
    Add('matr', ftInteger);
    Add('nVoo', ftString, 9);
    Add('cAerEmb', ftString, 4);
    Add('cAerDes', ftString, 4);
    Add('dVoo', ftDateTime);
    CreateDataSet;
  end;

  with CDSModalAqua, FieldDefs do
  begin
    Close;
    Clear;
    // Aquaviario
    Add('CNPJAgeNav', ftString, 18);
    Add('tpEmb', ftString, 2);
    Add('cEmbar', ftString, 10);
    Add('xEmbar', ftString, 60);
    Add('nViag', ftString, 10);
    Add('cPrtEmb', ftString, 5);
    Add('cPrtDest', ftString, 5);
    CreateDataSet;
  end;

  with CDSModalFerrov, FieldDefs do
  begin
    Close;
    Clear;
    // Ferroviário
    Add('xPref', ftString, 10);
    Add('dhTrem', ftDateTime);
    Add('xOri', ftString, 3);
    Add('xDest', ftString, 3);
    Add('qVag', ftInteger);
    CreateDataSet;
  end;

  with CDSModalFerrovVagoes, FieldDefs do
  begin
    Close;
    Clear;
    // Ferroviário - Vagoes
    Add('serie', ftString, 3);
    Add('nVag', ftInteger);
    Add('nSeq', ftInteger);
    Add('TU', ftCurrency);

    CreateDataSet;
  end;

  with cdsTermCarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('cTermCarreg', ftString, 8);
    Add('xTermCarreg', ftString, 60);
    CreateDataSet;
  end;

  with cdsTermDescarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('cTermDescarreg', ftString, 8);
    Add('xTermDescarreg', ftString, 60);
    CreateDataSet;
  end;

  with cdsEmbarcaComboio, FieldDefs do
  begin
    Close;
    Clear;
    Add('cEmbComb', ftString, 10);
    CreateDataSet;
  end;

  with cdsInfUnidCargaVazia, FieldDefs do
  begin
    Close;
    Clear;
    Add('idUnidCargaVazia', ftString, 20);
    Add('tpUnidCargaVazia', ftString, 20);
    CreateDataSet;
  end;

  with FMDFe do
  begin
    case Ide.modal of
      moRodoviario:
        CarregaModalRodoviario;
      moAereo:
        CarregaModalAereo;
      moAquaviario:
        CarregaModalAquaviario;
      moFerroviario:
        CarregaModalFerroviario;
    end;
  end;

end;

procedure TDMACBrMDFeDAMDFEFR.CarregaModalAereo;
begin
  with CDSModalAereo, FMDFe.aereo do
  begin
    Append;
    FieldByName('nac').AsInteger    := nac;
    FieldByName('matr').AsInteger   := matr;
    FieldByName('nVoo').AsString    := nVoo;
    FieldByName('cAerEmb').AsString := cAerEmb;
    FieldByName('cAerDes').AsString := cAerDes;
    FieldByName('dVoo').AsDateTime  := dVoo;
    Post;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaModalAquaviario;
begin
  with CDSModalAqua, FMDFe.aquav do
  begin
    Append;
    FieldByName('CNPJAgeNav').AsString := FormatarCNPJ(CNPJAgeNav);
    FieldByName('tpEmb').AsString      := tpEmb;
    FieldByName('cEmbar').AsString     := cEmbar;
    FieldByName('xEmbar').AsString     := xEmbar;
    FieldByName('nViag').AsString      := nViagem;
    FieldByName('cPrtEmb').AsString    := cPrtEmb;
    FieldByName('cPrtDest').AsString   := cPrtDest;
    Post;
  end;
end;


procedure TDMACBrMDFeDAMDFEFR.CarregaTermCarreg;
var
  i: integer;
begin
  with cdsTermCarrega, FMDFe.aquav.infTermCarreg do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cTermCarreg').AsString := Items[i].cTermCarreg;
      FieldByName('xTermCarreg').AsString := Items[i].xTermCarreg;
      Post;
    end;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaTermDescarreg;
var
  i: integer;
begin
  with cdsTermDescarrega, FMDFe.aquav.infTermDescarreg do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cTermDescarreg').AsString := Items[i].cTermDescarreg;
      FieldByName('xTermDescarreg').AsString := Items[i].xTermDescarreg;
      Post;
    end;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaEmbarcacaoComboio;
var
  i: integer;
begin
  with cdsEmbarcaComboio, FMDFe.aquav.infEmbComb do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cEmbComb').AsString := Items[i].cEmbComb;
      Post;
    end;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaInfUnidCargaVazia;
var
  i: integer;
begin
  with cdsInfUnidCargaVazia, FMDFe.aquav.infUnidCargaVazia do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('idUnidCargaVazia').AsString := Items[i].idUnidCargaVazia;
      case Items[i].tpUnidCargaVazia of
         ucContainer : FieldByName('tpUnidCargaVazia').AsString := '1 - Container';
         ucULD : FieldByName('tpUnidCargaVazia').AsString := '2 - Carreta';
      end;
      Post;
    end;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaModalFerroviario;
var
  i: integer;
begin
  with CDSModalFerrov, FMDFe.ferrov do
  begin
    Append;
    FieldByName('xPref').AsString    := xPref;
    FieldByName('dhTrem').AsDateTime := dhTrem;
    FieldByName('xOri').AsString     := xOri;
    FieldByName('xDest').AsString    := xDest;
    FieldByName('qVag').AsInteger    := qVag;
    Post;
  end;

  with CDSModalFerrovVagoes, FMDFe.ferrov.vag do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('serie').AsString := Items[i].serie;
      FieldByName('nVag').AsInteger := Items[i].nVag;
      FieldByName('nSeq').AsInteger := Items[i].nSeq;
      FieldByName('TU').AsCurrency  := Items[i].TU;
      Post;
    end;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaModalRodoviario;
var
  i: integer;
begin
  with cdsModalRodo, FMDFe.rodo do
  begin
    Append;
    FieldByName('RNTRC').AsString := RNTRC;
    FieldByName('CIOT').AsString  := CIOT;
    if veicTracao.placa <> '' then
    begin
      FieldByName('placa').AsString     := veicTracao.placa;
      FieldByName('RNTRCProp').AsString := veicTracao.prop.RNTRC;

      for i := 0 to veicTracao.condutor.Count - 1 do
      begin
        // Alteração proposta por Maciel Goettms (27/02/2014) Concatenação dos condutores já adicionados.
        FieldByName('CPF').AsString   := FieldByName('CPF').AsString + FormatarCPF(veicTracao.condutor.Items[i].CPF) + #13#10;
        FieldByName('xNome').AsString := FieldByName('xNome').AsString + veicTracao.condutor.Items[i].xNome + #13#10;
      end;
    end;
    for i := 0 to veicReboque.Count - 1 do
    begin
      FieldByName('placa').AsString     := FieldByName('placa').AsString + #13#10 + veicReboque.Items[i].placa;
      FieldByName('RNTRCProp').AsString := FieldByName('RNTRCProp').AsString + #13#10 + veicReboque.Items[i].prop.RNTRC;
    end;
    for i := 0 to valePed.disp.Count - 1 do
    begin
      FieldByName('CNPJForn').AsString := FieldByName('CNPJForn').AsString + FormatarCNPJ(valePed.disp.Items[i].CNPJForn) + #13#10;
      FieldByName('CNPJPg').AsString   := FieldByName('CNPJPg').AsString + FormatarCNPJ(valePed.disp.Items[i].CNPJPg) + #13#10;
      FieldByName('nCompra').AsString  := FieldByName('nCompra').AsString + valePed.disp.Items[i].nCompra + #13#10;
    end;
    Post;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaMunCarrega;
var i:integer;
begin
  with cdsMunCarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('xMunCarrega', ftString, 60);
    CreateDataSet;
    for i := 0 to FMDFe.Ide.infMunCarrega.Count - 1 do
    begin
      Append;
      FieldByName('xMunCarrega').AsString:=FMDFe.Ide.infMunCarrega[i].xMunCarrega;
    end;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaParametros;
begin
  with cdsParametros, FieldDefs do
  begin
    Close;
    Clear;
    Add('Versao', ftString, 5);
    Add('Imagem', ftString, 256);
    Add('Sistema', ftString, 150);
    Add('Usuario', ftString, 60);

    CreateDataSet;
    Append;

    FieldByName('Versao').AsString := '1.00';

    // Carregamento da imagem
    FieldByName('Imagem').AsString := Ifthen(DAMDFEClassOwner.Logo <> '', DAMDFEClassOwner.Logo,'');
    FieldByName('Sistema').AsString := Ifthen(DAMDFEClassOwner.Sistema <> '',DAMDFEClassOwner.Sistema,'Projeto ACBr - http://acbr.sf.net');
    FieldByName('Usuario').AsString := Ifthen(DAMDFEClassOwner.Usuario <> '',' - ' + DAMDFEClassOwner.Usuario,'');
    Post;

  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaPercurso;
var i:integer;
begin
  with cdsPercurso, FieldDefs do
  begin
    Close;
    Clear;
    Add('UFPer', ftString, 2);
    CreateDataSet;
    for i := 0 to FMDFe.Ide.infPercurso.Count - 1 do
    begin
      Append;
      FieldByName('UFPer').AsString:=FMDFe.Ide.infPercurso[i].UFPer;
    end;
  end;
end;

constructor TDMACBrMDFeDAMDFEFR.Create(AOwner: TComponent);
begin
  inherited;
  frxReport:= TfrxReport.Create(Self);
  frxReport.EngineOptions.UseGlobalDataSetList := False;
  with frxReport do
  begin
    //Version         := '5.4.3';
    DotMatrixReport           := False;
    IniFile                   := '\Software\Fast Reports';
    PreviewOptions.AllowEdit  := False;
    PreviewOptions.Buttons    := [pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick];
    PreviewOptions.Zoom       := 1;
    PrintOptions.Printer      := 'Default';
    PrintOptions.PrintOnSheet := 0;
    StoreInDFM                := False;
    OnGetValue                := frxReportGetValue;
  end;

  frxPDFExport:= TfrxPDFExport.Create(Self);
  with frxPDFExport do
  begin
    UseFileCache    := True;
    ShowProgress    := True;
    OverwritePrompt := False;
    DataOnly        := False;
    PrintOptimized  := True;
    Outline         := False;
    Background      := True;
    HTMLTags        := True;
    Quality         := 95;
    //Transparency    := False;
    Author          := 'FastReport';
    Subject         := 'Exportando DANFE para PDF';
    ProtectionFlags := [ePrint, eModify, eCopy, eAnnot];
    HideToolbar     := False;
    HideMenubar     := False;
    HideWindowUI    := False;
    FitWindow       := False;
    CenterWindow    := False;
    PrintScaling    := False;
  end;
  frxBarCodeObject:= TfrxBarCodeObject.Create(Self);

  FDAMDFEClassOwner := TACBrMDFeDAMDFeClass(AOwner);
end;

procedure TDMACBrMDFeDAMDFEFR.DataModuleCreate(Sender: TObject);
begin
	frxReport.PreviewOptions.Buttons := [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind,
    pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator, pbExportQuick];
end;

procedure TDMACBrMDFeDAMDFEFR.DataModuleDestroy(Sender: TObject);
begin
  frxPDFExport.Free;
  frxReport.Free;
  frxBarCodeObject.Free;
end;

procedure TDMACBrMDFeDAMDFEFR.frxReportGetValue(const VarName: string; var Value: Variant);
begin
  if VarName = 'CANCELADO' then
    Value := DAMDFEClassOwner.MDFeCancelada;
  if VarName = 'ENCERRADO' then
    Value := DAMDFEClassOwner.MDFeEncerrado;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaDadosEventos;
var
  i: integer;
begin
  with cdsEventos, FieldDefs do
  begin
    Close;

    Clear;
    Add('DescricaoTipoEvento', ftString, 150);
    Add('Modelo', ftString, 2);
    Add('Serie', ftString, 3);
    Add('Numero', ftString, 9);
    Add('MesAno', ftString, 5);
    Add('Barras', ftString, 44);
    Add('ChaveAcesso', ftString, 60);
    Add('cOrgao', ftInteger);
    Add('tpAmb', ftString, 100);
    Add('dhEvento', ftDateTime);
    Add('TipoEvento', ftString, 6);
    Add('DescEvento', ftString, 100);
    Add('nSeqEvento', ftInteger);
    Add('versaoEvento', ftString, 10);
    Add('cStat', ftInteger);
    Add('xMotivo', ftString, 100);
    Add('nProt', ftString, 20);
    Add('dhRegEvento', ftDateTime);
    Add('xJust', ftBlob);
    Add('xCondUso', ftBlob);
    Add('xCorrecao', ftBlob);
    Add('xNome', ftString, 60); // Condutor
    Add('CPF', ftString, 14);

    CreateDataSet;

    for i := 0 to FEvento.Evento.Count - 1 do
    begin
      Append;

      with Evento.Evento[i] do
      begin
        FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);

        // nota fiscal eletronica
        FieldByName('Modelo').AsString      := Copy(InfEvento.chMDFe, 21, 2);
        FieldByName('Serie').AsString       := Copy(InfEvento.chMDFe, 23, 3);
        FieldByName('Numero').AsString      := Copy(InfEvento.chMDFe, 26, 9);
        FieldByName('MesAno').AsString      := Copy(InfEvento.chMDFe, 05, 2) + '/' + Copy(InfEvento.chMDFe, 03, 2);
        FieldByName('Barras').AsString      := InfEvento.chMDFe;
        FieldByName('ChaveAcesso').AsString := FormatarChaveAcesso(InfEvento.chMDFe);

        // Carta de correção eletrônica
        FieldByName('cOrgao').AsInteger := InfEvento.cOrgao;

        case InfEvento.tpAmb of
          taProducao:
            FieldByName('tpAmb').AsString := 'PRODUÇÃO';
          taHomologacao:
            begin
              FieldByName('tpAmb').AsString      := 'HOMOLOGAÇÃO - SEM VALOR FISCAL';
              frxReport.Variables['HOMOLOGACAO'] := True;
            end;
        end;

        FieldByName('dhEvento').AsDateTime    := InfEvento.dhEvento;
        FieldByName('TipoEvento').AsString    := InfEvento.TipoEvento;
        FieldByName('DescEvento').AsString    := InfEvento.DescEvento;
        FieldByName('nSeqEvento').AsInteger   := InfEvento.nSeqEvento;
        FieldByName('versaoEvento').AsString  := InfEvento.versaoEvento;
        FieldByName('cStat').AsInteger        := RetInfEvento.cStat;
        FieldByName('xMotivo').AsString       := RetInfEvento.xMotivo;
        FieldByName('nProt').AsString         := RetInfEvento.nProt;
        FieldByName('dhRegEvento').AsDateTime := RetInfEvento.dhRegEvento;
        FieldByName('xJust').AsString         := InfEvento.detEvento.xJust;
        FieldByName('xNome').AsString         := InfEvento.detEvento.xNome;
        FieldByName('CPF').AsString           := FormatarCPF(InfEvento.detEvento.CPF);

      end;

      Post;
    end;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaDocumentos;
var
  i, j, x, y: integer;
begin
  with CDSDocumentos, FieldDefs do
  begin
    Close;
    Clear;
    Add('Tipo', ftString, 5);
    Add('Chave', ftString, 70);
    Add('idUnidTransp', ftString, 70);
    Add('idUnidCarga', ftString, 70);
    CreateDataSet;

    with FMDFe.infDoc do
    begin
      for i := 0 to infMunDescarga.Count - 1 do
      begin
        with infMunDescarga.Items[i] do
        begin

          for j := 0 to infCTe.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'CTe';
            FieldByName('Chave').AsString := FormatarChaveAcesso(infCTe.Items[j].chCTe);
            Post;
            with infCTe[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infCT.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'CT';
            FieldByName('Chave').AsString := FormatarCNPJ(FMDFe.emit.CNPJ) + '        ' +
              IntToStr(infCT.Items[j].serie) + '-' + infCT.Items[j].nCT;
            Post;
            with infCT[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infNFe.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'NFe';
            FieldByName('Chave').AsString := FormatarChaveAcesso(infNFe.Items[j].chNFe);
            Post;
            with infNFe[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infNF.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'NF';
            FieldByName('Chave').AsString := FormatarCNPJ(FMDFe.emit.CNPJ) + '        ' +
              IntToStr(infNF.Items[j].serie) + '-' + IntToStr(infNF.Items[j].nNF);
            Post;
            with infNF[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infMDFeTransp.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'MDF-e';
            FieldByName('Chave').AsString := FormatarChaveAcesso(infMDFeTransp.Items[j].chMDFe);
            Post;
            with infMDFeTransp[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaEmitente;
begin
  with cdsEmitente, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('IE', ftString, 14);
    Add('xNome', ftString, 60);
    Add('xFant', ftString, 60);
    Add('xLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('xCpl', ftString, 60);
    Add('xBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('xMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('Fone', ftString, 15);
    Add('email', ftString, 60);
    Add('site', ftString, 60);

    CreateDataSet;
    Append;
    with FMDFe.emit do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJ);
      FieldByName('IE').AsString    := IE;
      FieldByName('XNome').AsString := xNome;
      FieldByName('XFant').AsString := XFant;
      with EnderEmit do
      begin
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := FormatarCEP(CEP);
        FieldByName('Fone').AsString    := FormatarFone(Fone);
        FieldByName('email').AsString   := email;
        FieldByName('site').AsString    := FDAMDFEClassOwner.Site;
      end;
    end;
    Post;

  end;
end;

function SubstrCount(const ASubString, AString: string): Integer;
var
  i: integer;
begin
  Result := -1;
  i := 0;
  repeat
    Inc(Result);
    i := PosEx(ASubString, AString, i + 1);
  until i = 0;
end;

function Split(const ADelimiter, AString: string): TSplitResult;
var
  Step: ^string;
  Chr: PChar;
  iPos, iLast, iDelLen, iLen, x: integer;
label
  EndLoop;
begin
  SetLength(Result, SubstrCount(ADelimiter, AString) + 1);
  if High(Result) = 0 then
    Result[0] := AString
  else
  begin
    iDelLen := PCardinal(Cardinal(ADelimiter) - SizeOf(Cardinal))^;
    iLen := PCardinal(Cardinal(AString) - SizeOf(Cardinal))^;
    Step := @Result[0];
    iLast := 0;
    iPos := 0;
    repeat
      if iPos + iDelLen > iLen then
      begin
        if iLast <> iPos then
          iPos := iLen;
      end else
        for x := 1 to iDelLen do
          if AString[iPos + x] <> ADelimiter[x] then
            goto EndLoop;

      if iPos - iLast > 0 then
      begin
        SetLength(Step^, iPos - iLast);
        Chr := PChar(Step^);
        for x := 1 to PCardinal(Cardinal(Step^) - SizeOf(Cardinal))^ do
        begin
          Chr^ := AString[iLast + x];
          Inc(Chr);
        end;
      end else
        Step^ := '';

      Cardinal(Step) := Cardinal(Step) + SizeOf(Cardinal);
      iLast := iPos + iDelLen;

      EndLoop:
      Inc(iPos);
    until iLast >= iLen;
  end;
end;

procedure TDMACBrMDFeDAMDFEFR.CarregaIdentificacao;
var
  vTemp:TStringList;
  wObs:String;
  Campos:TSplitResult;
  IndexCampo:Integer;
  TmpStr:String;
  BufferObs:String;
begin
  with cdsIdentificacao, FieldDefs do
  begin
    Close;
    Clear;
    Add('Id', ftString, 44);
    Add('Chave', ftString, 60);
    Add('Protocolo', ftString, 120);
    Add('tpAmb', ftInteger);
    Add('tpEmit', ftInteger);
    Add('Modelo', ftString, 5);
    Add('serie', ftString, 3);
    Add('nMDF', ftInteger);
    Add('modal', ftInteger);
    Add('dhEmi', ftDateTime);
    Add('tpEmis', ftInteger);
    Add('UFIni', ftString, 2);
    Add('UFFim', ftString, 2);
    Add('OBS', ftMemo);

    // Totalização
    Add('qCTe', ftInteger);
    Add('qCT', ftInteger);
    Add('qNFe', ftInteger);
    Add('qNF', ftInteger);
    Add('qMDFe', ftInteger);
    Add('qCarga', ftCurrency);

    CreateDataSet;

    Append;

    with FMDFe.infMDFe do
    begin
      FieldByName('Id').AsString    := OnlyNumber(Id);
      FieldByName('Chave').AsString := FormatarChaveAcesso(Id);
    end;

    with FMDFe.Ide do
    begin
      FieldByName('tpAmb').AsInteger  := StrToIntDef(TpAmbToStr(tpAmb), 0);
      FieldByName('tpEmit').AsInteger := StrToIntDef(TpEmitenteToStr(tpEmit), 0);
      FieldByName('Modelo').AsString  := modelo;
      FieldByName('serie').AsString   := Poem_Zeros(serie, 3);
      FieldByName('nMDF').AsInteger   := nMDF;
      FieldByName('modal').AsInteger  := StrToIntDef(ModalToStr(modal), 0);
      FieldByName('dhEmi').AsDateTime := dhEmi;
      FieldByName('tpEmis').AsInteger := StrToIntDef(TpEmisToStr(tpEmis), 0);
      FieldByName('UFIni').AsString   := UFIni;
      FieldByName('UFFim').AsString   := UFFim;
      if (tpEmis = teNormal) or (not EstaVazio(FDAMDFEClassOwner.ProtocoloMDFE)) or (not EstaVazio(FMDFe.procMDFe.nProt))
      then
      begin
        if not EstaVazio(FDAMDFEClassOwner.ProtocoloMDFE) then
          FieldByName('Protocolo').AsString := FDAMDFEClassOwner.ProtocoloMDFE
        else if not EstaVazio(FMDFe.procMDFe.nProt) then
          FieldByName('Protocolo').AsString := FMDFe.procMDFe.nProt + '   ' +
            IfThen(FMDFe.procMDFe.dhRecbto <> 0, DateTimeToStr(FMDFe.procMDFe.dhRecbto), '')
        else
          FieldByName('Protocolo').AsString := 'MDFe sem Autorização de Uso da SEFAZ';
      end
      else
        FieldByName('Protocolo').AsString := 'Impressão em contingência. Obrigatória a autorização em 168 horas' +
          ' após esta impressão (' + FormatDateTimeBr(dhEmi) + ')';

    end;
    with FMDFe.tot do
    begin
      FieldByName('qCTe').AsInteger    := qCTe;
      FieldByName('qCT').AsInteger     := qCT;
      FieldByName('qNFe').AsInteger    := qNFe;
      FieldByName('qNF').AsInteger     := qNF;
      FieldByName('qMDFe').AsInteger   := qMDFe;
      if cUnid = uTon then
        FieldByName('qCarga').AsCurrency := qCarga * 1000
      else
        FieldByName('qCarga').AsCurrency := qCarga;
    end;

    // Incluido por Paulo Hostert em 18/11/2014.
    wObs := FMDFe.infAdic.infCpl;
    vTemp := TStringList.Create;
    try
      if Trim(wObs) <> '' then
      begin
        Campos := Split(';', wObs);
        for IndexCampo := 0 to Length(Campos) - 1 do
          vTemp.Add(Trim(Campos[IndexCampo]));

        TmpStr := vTemp.Text;
        BufferObs := TmpStr;
      end
      else
        BufferObs := '';
    finally
      vTemp.Free;
    end;
    FieldByName('OBS').AsString := BufferObs;

    Post;
  end;
end;

function CollateBr(Str: string): string;
var
  Resultado, Temp: string;
  vChar          : Char;
  Tamanho, i     : integer;
begin
  Result  := '';
  Tamanho := Length(Str);
  i       := 1;
  while i <= Tamanho do
  begin
    Temp  := Copy(Str, i, 1);
    vChar := Temp[1];
    case vChar of
      'á', 'â', 'ã', 'à', 'ä', 'å', 'Á', 'Â', 'Ã', 'À', 'Ä', 'Å':
        Resultado := 'A';
      'é', 'ê', 'è', 'ë', 'É', 'Ê', 'È', 'Ë':
        Resultado := 'E';
      'í', 'î', 'ì', 'ï', 'Í', 'Î', 'Ì', 'Ï':
        Resultado := 'I';
      'ó', 'ô', 'õ', 'ò', 'ö', 'Ó', 'Ô', 'Õ', 'Ò', 'Ö':
        Resultado := 'O';
      'ú', 'û', 'ù', 'ü', 'Ú', 'Û', 'Ù', 'Ü':
        Resultado := 'U';
      'ç', 'Ç':
        Resultado := 'C';
      'ñ', 'Ñ':
        Resultado := 'N';
      'ý', 'ÿ', 'Ý', 'Y':
        Resultado := 'Y';
    else
      if vChar > #127 then
        Resultado := #32

{$IFDEF DELPHI12_UP}
      else if CharInset(vChar, ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', ' ']) then

{$ELSE}
      else if vChar in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', ' '] then

{$ENDIF}
        Resultado := UpperCase(vChar);
    end;
    Result := Result + Resultado;
    i      := i + 1;
  end;
end;
procedure TDMACBrMDFeDAMDFEFR.SetDataSetsToFrxReport;
begin
  frxReport.DataSets.Clear;
  frxReport.DataSets.Add(frxIdentificacao);
  frxReport.DataSets.Add(frxEmitente);
  frxReport.DataSets.Add(frxMunCarrega);
  frxReport.DataSets.Add(frxModalRodo);
  frxReport.DataSets.Add(frxModalAereo);
  frxReport.DataSets.Add(frxModalAqua);
  frxReport.DataSets.Add(frxModalFerrov);
  frxReport.DataSets.Add(frxModalFerrovVagoes);
  frxReport.DataSets.Add(frxDocumentos);
  frxReport.DataSets.Add(frxParametros);
  frxReport.DataSets.Add(frxPercurso);
  frxReport.DataSets.Add(frxEventos);
  frxReport.DataSets.Add(frxTermCarrega);
  frxReport.DataSets.Add(frxTermDescarrega);
  frxReport.DataSets.Add(frxEmbarcaComboio);
  frxReport.DataSets.Add(frxInfUnidCargaVazia);

  frxReport.EnabledDataSets.Clear;
  frxReport.EnabledDataSets.Add(frxIdentificacao);
  frxReport.EnabledDataSets.Add(frxEmitente);
  frxReport.EnabledDataSets.Add(frxMunCarrega);
  frxReport.EnabledDataSets.Add(frxModalRodo);
  frxReport.EnabledDataSets.Add(frxModalAereo);
  frxReport.EnabledDataSets.Add(frxModalAqua);
  frxReport.EnabledDataSets.Add(frxModalFerrov);
  frxReport.EnabledDataSets.Add(frxModalFerrovVagoes);
  frxReport.EnabledDataSets.Add(frxDocumentos);
  frxReport.EnabledDataSets.Add(frxParametros);
  frxReport.EnabledDataSets.Add(frxPercurso);
  frxReport.EnabledDataSets.Add(frxEventos);
  frxReport.EnabledDataSets.Add(frxTermCarrega);
  frxReport.EnabledDataSets.Add(frxTermDescarrega);
  frxReport.EnabledDataSets.Add(frxEmbarcaComboio);
  frxReport.EnabledDataSets.Add(frxInfUnidCargaVazia);
end;

end.
