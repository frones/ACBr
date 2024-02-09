{******************************************************************************}
{ Projeto: Componente ACBrBPe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Bilhete de }
{ Passagem Eletrônica - BPe                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2017                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

{$I ACBr.inc}

unit ACBrBPeDABPEFR;

interface

uses
  ACBrBPeDABPEClass, 
  pcnBPe, 
  frxClass, 
  Classes, 
  frxExportPDF, 
  frxBarcode,
  DBClient, 
  DB, 
  frxDBSet, 
  SysUtils, 
  ACBrValidador,
  ACBrUtil.Strings, 
  ACBrBPe, 
  pcnConversao, 
  StrUtils, 
  pcnConversaoBPe,
  Graphics, 
  ACBrDFe;

type
  EACBrBPeDABPEFR = class(Exception);

  TACBrBPeDABPEFR = class(TACBrBPeDABPEClass)
  private
    FDABPEClassOwner: TACBrBPeDABPEClass;
    FBPe: TBPe;
    FSelecionaImpressora: Boolean;
    FFastFile: string;

    cdsEmitente: TClientDataSet;
    cdsParametros: TClientDataSet;
    cdsInfPassagem: TClientDataSet;
    cdsInfViagem: TClientDataSet;
    cdsAgencia: TClientDataSet;
    cdsInfValorBPe: TClientDataSet;
    cdsInfBPe: TClientDataSet;
    cdsIde: TClientDataSet;
    cdsPag: TClientDataSet;
    cdsProcBPe: TClientDataSet;
    cdsInfAdic: TClientDataSet;
    cdsImp: TClientDataSet;
    cdsInfBPeSupl: TClientDataSet;

    frxEmitente: TfrxDBDataset;
    frxParametros: TfrxDBDataset;
    frxInfPassagem: TfrxDBDataset;
    frxInfViagem: TfrxDBDataset;
    frxAgencia: TfrxDBDataset;
    frxInfValorBPe: TfrxDBDataset;
    frxInfBPe: TfrxDBDataset;
    frxIde: TfrxDBDataset;
    frxPag: TfrxDBDataset;
    frxProcBPe: TfrxDBDataset;
    frxInfAdic: TfrxDBDataset;
    frxImp: TfrxDBDataset;
    frxInfBPeSupl: TfrxDBDataset;

    procedure CarregaEmitente;
    procedure CarregaParametros;
    procedure CarregaInfPassagem;
    procedure CarregaInfViagem;
    procedure CarregaAgencia;
    procedure CarregaInfValorBPe;
    procedure CarregaInfBPe;
    procedure CarregaIde;
    procedure CarregaPag;
    procedure CarregaProcBPe;
    procedure CarregaInfAdic;
    procedure CarregaImp;
    procedure CarregaInfBpeSupl;

    function GetPreparedReport: TfrxReport;
    function PrepareReport(BPe: TBPe = nil): Boolean;
    procedure CriarDataSetsFrx;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
  public
    frxReport: TfrxReport;
    frxPDFExport: TfrxPDFExport;
    frxBarCodeObject: TfrxBarCodeObject;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetDataSetsToFrxReport;
    procedure frxReportGetValue(const VarName: string; var Value: Variant);
    procedure LimpaDados;
    procedure CarregaDados;

    procedure ImprimirDABPE(BPe: TBPe = nil); override;
    procedure ImprimirDABPEPDF(BPe: TBPe = nil); override;

    property DABPEClassOwner: TACBrBPeDABPEClass read FDABPEClassOwner;
  published
    property FastFile: string read FFastFile write FFastFile;
    property SelecionaImpressora: Boolean read FSelecionaImpressora write FSelecionaImpressora;
    property PreparedReport: TfrxReport read GetPreparedReport;
  end;

implementation

uses
  ACBrDFeUtil, ACBrDelphiZXingQRCode, ACBrImage;

{ TACBrBPeDABPEFR }

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

procedure TACBrBPeDABPEFR.CarregaDados;
begin
  LimpaDados;
  CarregaEmitente;
  CarregaParametros;
  CarregaInfPassagem;
  CarregaInfViagem;
  CarregaAgencia;
  CarregaInfValorBPe;
  CarregaInfBPe;
  CarregaIde;
  CarregaPag;
  CarregaProcBPe;
  CarregaInfAdic;
  CarregaImp;
  CarregaInfBpeSupl;
end;

procedure TACBrBPeDABPEFR.CarregaAgencia;
begin
  if trim(FBPe.Agencia.xNome) <> '' then
  begin
    with cdsAgencia do
    begin
		Append;
		with FBPe.Agencia do
		begin
			FieldByName('xNome').AsString := xNome;
			FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJ);

			with EnderAgencia do
			begin
				FieldByName('xLgr').AsString    := xLgr;
				FieldByName('Nro').AsString     := nro;
				FieldByName('xBairro').AsString := xBairro;
				FieldByName('xMun').AsString    := xMun;
				FieldByName('UF').AsString      := UF;
			end;
		end;
		Post;
    end;
  end;
end;

procedure TACBrBPeDABPEFR.CarregaEmitente;
begin
    with cdsEmitente do
    begin
        Append;
        with FBPe.emit do
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
                FieldByName('CEP').AsString     := FormatarCEP(Poem_Zeros(CEP, 8));
                FieldByName('Fone').AsString    := FormatarFone(Fone);
                FieldByName('email').AsString   := email;
            end;
        end;
        Post;
    end;
end;

procedure TACBrBPeDABPEFR.CarregaIde;
begin
    with cdsIde do
    begin
        Append;
        with FBPe.Ide do
        begin
            FieldByName('nBp').AsInteger := nBP;
            FieldByName('serie').AsInteger := serie;
            FieldByName('DataEmi').AsString := FormatDateTime('dd/mm/yyyy', dhEmi);
            FieldByName('HoraEmi').AsString := FormatDateTime('hh:nn:ss', dhEmi);
            FieldByName('tpAmb').AsString := TpAmbToStr(tpAmb);
            FieldByName('UFIni').AsString := UFIni;
            FieldByName('UFFim').AsString := UFFim;
        end;
        post;
    end;
end;

procedure TACBrBPeDABPEFR.CarregaImp;
begin
    with cdsImp do
    begin
        Append;
        with FBPe.Imp do
        begin
            FieldByName('vTotTrib').AsFloat := vTotTrib;
            FieldByName('infAdFisco').AsString := StringReplace(infAdFisco, '|', sLineBreak, [rfReplaceAll]);
        end;
        Post;
    end;
end;

procedure TACBrBPeDABPEFR.CarregaInfAdic;
begin
    with cdsInfAdic do
    begin
        Append;
        with FBPe.InfAdic do
        begin
            FieldByName('infAdFisco').AsString := StringReplace(infAdFisco, '|', sLineBreak, [rfReplaceAll]);
            FieldByName('infCpl').AsString := infCpl;
        end;
        Post;
    end;
end;

procedure TACBrBPeDABPEFR.CarregaInfBPe;
begin
    with cdsInfBPe do
    begin
        Append;
        with FBPe.infBPe do
        begin
            FieldByName('ID').AsString := ID;
            FieldByName('ChaveBPe').AsString := OnlyNumber(ID);
        end;
        Post;
    end;
end;


procedure TACBrBPeDABPEFR.CarregaInfPassagem;
begin
    with cdsInfPassagem do
    begin
        Append;
        with FBPe.infPassagem do
        begin
            FieldByName('xLocOrig').AsString := xLocOrig;
            FieldByName('xLocDest').AsString := xLocDest;
            FieldByName('DataEmb').AsString  := FormatDateTime('dd/mm/yyyy', dhEmb);
            FieldByName('HoraEmb').AsString  := FormatDateTime('hh:nn', dhEmb);

            with infPassageiro do
            begin
                FieldByName('nDoc').AsString := nDoc;
                FieldByName('xNome').AsString := xNome;
            end;
        end;
        Post;
    end;
end;

procedure TACBrBPeDABPEFR.CarregaInfValorBPe;
var
    I: Integer;
begin
    with cdsInfValorBPe do
    begin
        for I := 0 to FBPe.infValorBPe.Comp.Count - 1 do
        begin
            Append;
            with FBPe.infValorBPe do
            begin
                FieldByName('vBP').AsFloat        := vBP;
                FieldByName('vDesconto').AsFloat  := vDesconto;
                FieldByName('vTroco').AsFloat     := vTroco;
                FieldByName('vPgto').AsFloat      := vPgto;
                FieldByName('xDesconto').AsString := xDesconto;

                with Comp do
                begin
                    FieldByName('tpComp').AsString := tpComponenteToDesc(Items[I].tpComp);
                    FieldByName('vComp').AsFloat := Items[I].vComp;
                end;
            end;
            Post;
        end;
    end;
end;

procedure TACBrBPeDABPEFR.CarregaInfViagem;
begin
    with cdsInfViagem do
    begin
        Append;
        with FBPe.infViagem.Items[0] do
        begin
            FieldByName('xPercurso').AsString  := xPercurso;
            FieldByName('Plataforma').AsString := Plataforma;
            FieldByName('Poltrona').AsInteger  := Poltrona;
            FieldByName('tpServico').AsString  := tpServicoToDesc(tpServ);
            FieldByName('Prefixo').AsString    := Prefixo;
        end;
        Post;
    end;
end;

procedure TACBrBPeDABPEFR.CarregaPag;
var
    I: Integer;
begin
    with cdsPag do
    begin
        with FBPe.Pag do
        begin
            if FBPe.Pag.Count > 0 then
            begin
                for I := 0 to FBPe.Pag.Count - 1 do
                begin
                    Append;
                    FieldByName('tPag').AsString := FormaPagamentoBPeToDescricao(Items[I].tPag);
                    FieldByName('vPag').AsFloat  := Items[I].vPag;
                    Post;
                end;
            end;
        end;
    end;
end;

procedure TACBrBPeDABPEFR.CarregaParametros;
var
  //vChave_Contingencia: string;
  //vResumo            : string;
  vStream            : TMemoryStream;
  vStringStream      : TStringStream;
begin
  { parâmetros }
  with cdsParametros do
  begin
    Append;

    {$IFDEF PL_103}
      FieldByName('Versao').AsString := '1.03';
    {$ENDIF}
    {$IFDEF PL_104}
      FieldByName('Versao').AsString := '1.04';
    {$ENDIF}
    {$IFDEF PL_200}
      if FBPe.infBPe.versao = 2 then
        FieldByName('Versao').AsString := '2.00'
      else
        FieldByName('Versao').AsString := '3.00';
    {$ENDIF}

    if (FBPe.ide.tpAmb = taHomologacao) and (FBPe.ide.TpEmis in [teNormal]) then
      FieldByName('Mensagem0').AsString := 'BPe sem Valor Fiscal - HOMOLOGAÇÃO'
    else
    if (FBPe.ide.tpAmb = taHomologacao) and (FBPe.ide.TpEmis = teOffLine) then
      FieldByName('Mensagem0').AsString := 'HOMOLOGAÇÃO - Emitido em OFFline'
    else
    begin
      if not(FBPe.ide.TpEmis in [teOffLine]) then
      begin
        if ((EstaVazio(Protocolo)) and (EstaVazio(FBPe.procBPe.nProt))) then
          FieldByName('Mensagem0').AsString := 'BPe sem Autorização de Uso da SEFAZ'
        else
        if (not((EstaVazio(Protocolo)) and (EstaVazio(FBPe.procBPe.nProt)))) and
           (FBPe.procBPe.cStat = 101)
        then
          FieldByName('Mensagem0').AsString := 'Bpe Cancelado'
        else
        begin
          if Cancelada then
            FieldByName('Mensagem0').AsString := 'Bpe Cancelado'
          else
            FieldByName('Mensagem0').AsString := '';
        end;
      end
      else
        FieldByName('Mensagem0').AsString := '';
    end;

    // Carregamento da imagem
    if DABPEClassOwner.Logo <> '' then
    begin
      FieldByName('Imagem').AsString := DABPEClassOwner.Logo;
      vStream                        := TMemoryStream.Create;
      try
        if FileExists(DABPEClassOwner.Logo) then
          vStream.LoadFromFile(DABPEClassOwner.Logo)
        else
        begin
          vStringStream := TStringStream.Create(DABPEClassOwner.Logo);
          try
            vStream.LoadFromStream(vStringStream);
          finally
            vStringStream.Free;
          end;
        end;
        vStream.Position := 0;
        TBlobField(cdsParametros.FieldByName('LogoCarregado')).LoadFromStream(vStream);
      finally
        vStream.Free;
      end;
    end;

    FieldByName('Sistema').AsString := Sistema;
    FieldByName('Usuario').AsString := Ifthen(Usuario <> '', ' - ' + Usuario,'');
    
    FieldByName('Site').AsString  := Site;
    FieldByName('Email').AsString := Email;

    {if ImprimirDescPorc then
      FieldByName('Desconto').AsString := 'DESC %'
    else
      FieldByName('Desconto').AsString := 'V.DESC.';}

    if ((FBPe.ide.TpEmis = teNormal) ) then
    begin
      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
      FieldByName('Contingencia_ID').AsString       := '';

      if ((Cancelada) or (FBPe.procBPe.cStat = 101)) then
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO'
      else
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

      if EstaVazio(Protocolo) then
      begin
        if not(FBPe.ide.TpEmis in [teOffLine]) and EstaVazio(FBPe.procBPe.nProt) then
          FieldByName('Contingencia_Valor').AsString := 'BPe sem Autorização de Uso da SEFAZ'
        else
          FieldByName('Contingencia_Valor').AsString := FBPe.procBPe.nProt + ' ' + IfThen(FBPe.procBPe.dhRecbto <> 0,
            DateTimeToStr(FBPe.procBPe.dhRecbto), '');
      end
      else
        FieldByName('Contingencia_Valor').AsString := Protocolo;
    end
    else
    begin
      //vChave_Contingencia                           := TACBrBPe(DABPEClassOwner.ACBrBPe).GerarChaveContingencia(FBPe);
      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
      //FieldByName('Contingencia_ID').AsString       := vChave_Contingencia;

      if ((FBPe.ide.TpEmis = teOffLine) ) then
      begin
        FieldByName('Contingencia_Descricao').AsString := 'DADOS DA BP-E';
        //FieldByName('Contingencia_Valor').AsString     := FormatarChaveAcesso(vChave_Contingencia);
      end;
      {
      else
      if (FBPe.ide.TpEmis = teSVCSP) or (FBPe.ide.TpEmis = teSVCRS) then
      begin
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';
        FieldByName('Contingencia_Valor').AsString     := FBPe.procBPe.nProt + ' ' + IfThen(FBPe.procBPe.dhRecbto <> 0,
                                                                                            DateTimeToStr(FBPe.procBPe.dhRecbto), '');
      end;}
    end;

    FieldByName('URL').AsString := TACBrBPe(DABPEClassOwner.ACBrBPe).GetURLConsultaBPe(FBPe.ide.cUF, FBPe.Ide.tpAmb);
    Post;
  end;
end;

procedure TACBrBPeDABPEFR.CarregaProcBPe;
begin
    with cdsProcBPe do
    begin
        Append;
        with FBPe.procBPe do
        begin
            FieldByName('nProt').AsString := nProt;
            FieldByName('DataAut').AsString := FormatDateTime('dd/mm/yyyy', dhRecbto);
            FieldByName('HoraAut').AsString := FormatDateTime('hh:nn:ss', dhRecbto);
        end;
        Post;
    end;
end;

constructor TACBrBPeDABPEFR.Create(AOwner: TComponent);
begin
    inherited create(AOwner);
    FDABPEClassOwner := TACBrBPeDABPEClass(Self);
    FFastFile       := '';
//    FEspessuraBorda := 1;
    CriarDataSetsFrx;
    SetDataSetsToFrxReport;
end;

procedure TACBrBPeDABPEFR.CriarDataSetsFrx;
begin
    frxReport := TfrxReport.Create(nil);
   	frxReport.PreviewOptions.Buttons := [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind,
      pbOutline, pbPageSetup, pbTools, pbNavigator, pbExportQuick ];
    frxReport.EngineOptions.UseGlobalDataSetList := False;
    with frxReport do
    begin
        ScriptLanguage := 'PascalScript';
        StoreInDFM     := False;
        OnBeforePrint  := frxReportBeforePrint;
        OnReportPrint  := 'frxReportOnReportPrint';
        PreviewOptions.Buttons :=[pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick];
        OnGetValue := frxReportGetValue;
    end;

    frxPDFExport := TfrxPDFExport.Create(nil);
    with frxPDFExport do
    begin
        Background     := True;
        PrintOptimized := True;
        Subject        := 'Exportando BPe para PDF';
    end;

    RttiSetProp(frxPDFExport, 'Transparency', 'False');

    // CDS
    cdsEmitente := TClientDataSet.Create(Self);
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
        CreateDataSet;
    end;

    cdsParametros := TClientDataSet.Create(Self);
    with cdsParametros, FieldDefs do
    begin
        Close;
        Clear;
        Add('ResumoCanhoto', ftString, 200);
        Add('Mensagem0', ftString, 60);
        Add('Versao', ftString, 5);
        Add('Imagem', ftString, 256);
        Add('Sistema', ftString, 150);
        Add('Usuario', ftString, 60);
        Add('Site', ftString, 60);
        Add('Email', ftString, 60);
        Add('Desconto', ftString, 60);
        Add('ChaveAcesso_Descricao', ftString, 90);
        Add('Contingencia_ID', ftString, 36);
        Add('Contingencia_Descricao', ftString, 60);
        Add('Contingencia_Valor', ftString, 60);
        Add('PrintCanhoto',ftString,1);
        Add('LinhasPorPagina', ftInteger);
        Add('LogoCarregado', ftBlob);
        Add('URL', ftString, 400);
        CreateDataSet;
    end;

    cdsInfPassagem := TClientDataSet.Create(Self);
    with cdsInfPassagem, FieldDefs do
    begin
        Close;
        Clear;
        Add('xLocOrig', ftString, 60);
        Add('xLocDest', ftString, 60);
        Add('DataEmb', ftDateTime);
        Add('HoraEmb', ftString, 5);
        Add('nDoc', ftString, 20);
        Add('xNome', ftString, 60);
        CreateDataSet;
    end;

    cdsInfViagem := TClientDataSet.Create(Self);
    with cdsInfViagem, FieldDefs do
    begin
        Close;
        Clear;
        Add('xPercurso', ftString, 100);
        Add('Plataforma', ftString, 10);
        Add('Poltrona', ftInteger);
        Add('tpServico', ftString, 40);
        Add('Prefixo', ftString, 20);
        CreateDataSet;
    end;

    cdsAgencia := TClientDataSet.Create(Self);
    with cdsAgencia, FieldDefs do
    begin
        Close;
        Clear;
        Add('xNome', ftString, 60);
        Add('CNPJ', ftString, 18);
        Add('xLgr', ftString, 60);
        Add('Nro', ftString, 60);
        Add('xBairro', ftString, 60);
        Add('xMun', ftString, 60);
        Add('UF', ftString, 2);
        CreateDataSet;
    end;

    cdsInfValorBPe := TClientDataSet.Create(Self);
    with cdsInfValorBPe, FieldDefs do
    begin
        Close;
        Clear;
        Add('vBP', ftFloat);
        Add('vDesconto', ftFloat);
        Add('vTroco', ftFloat);
        Add('vPgto', ftFloat);
        Add('xDesconto', ftString, 100);
        Add('tpComp', ftString, 20);
        Add('vComp', ftFloat);
        CreateDataSet;
    end;

    cdsInfBPe := TClientDataSet.Create(Self);
    with cdsInfBPe, FieldDefs do
    begin
        Close;
        Clear;
        Add('ID', ftString, 47);
        Add('ChaveBPe', ftString, 44);
        CreateDataSet;
    end;

    cdsIde := TClientDataSet.Create(Self);
    with cdsIde, FieldDefs do
    begin
        Close;
        Clear;
        Add('nBp', ftInteger);
        Add('serie', ftInteger);
        Add('DataEmi', ftDateTime);
        Add('HoraEmi', ftString, 8);
        Add('tpAmb', ftString, 1);
        Add('UFIni', ftString, 2);
        Add('UFFim', ftString, 2);
        CreateDataSet;
    end;

    cdsPag := TClientDataSet.Create(Self);
    with cdsPag, FieldDefs do
    begin
        Close;
        Clear;
        Add('tPag', ftString, 50);
        Add('vPag', ftFloat);
        CreateDataSet;
    end;

    cdsProcBPe := TClientDataSet.Create(Self);
    with cdsProcBPe, FieldDefs do
    begin
        Close;
        Clear;
        Add('nProt', ftString, 15);
        Add('DataAut', ftDateTime);
        Add('HoraAut', ftString, 8);
        CreateDataSet;
    end;

    cdsInfAdic := TClientDataSet.Create(Self);
    with cdsInfAdic, FieldDefs do
    begin
        Close;
        Clear;
        Add('infAdFisco', ftString, 2000);
        Add('infCpl', ftString, 5000);
        CreateDataSet;
    end;

    cdsImp := TClientDataSet.Create(Self);
    with cdsImp, FieldDefs do
    begin
        Close;
        Clear;
        Add('vTotTrib', ftFloat);
        Add('infAdFisco', ftString, 2000);
        CreateDataSet;
    end;

    //By Sandro
    cdsInfBPeSupl := TClientDataSet.Create(Self);
    with cdsInfBPeSupl, FieldDefs do
    begin
      Close;
      Clear;
      Add('qrCodBPe', ftString, 1000);
      Add('boardPassBPe', ftString, 1000);
      CreateDataSet;
    end;



    // FRX
    frxEmitente := TfrxDBDataset.Create(Self);
    with frxEmitente do
    begin
        UserName       := 'Emitente';
        OpenDataSource := False;
        DataSet        := cdsEmitente;
    end;

    frxParametros := TfrxDBDataset.Create(Self);
    with frxParametros do
    begin
        UserName       := 'Parametros';
        OpenDataSource := False;
        DataSet        := cdsParametros;
    end;

    frxInfPassagem := TfrxDBDataset.Create(Self);
    with frxInfPassagem do
    begin
        UserName       := 'InfPassagem';
        OpenDataSource := False;
        DataSet        := cdsInfPassagem;
    end;

    frxInfViagem := TfrxDBDataset.Create(Self);
    with frxInfViagem do
    begin
        UserName       := 'InfViagem';
        OpenDataSource := False;
        DataSet        := cdsInfViagem;
    end;

    frxAgencia := TfrxDBDataset.Create(Self);
    with frxAgencia do
    begin
        UserName       := 'Agencia';
        OpenDataSource := False;
        DataSet        := cdsAgencia;
    end;

    frxInfValorBPe := TfrxDBDataset.Create(Self);
    with frxInfValorBPe do
    begin
        UserName       := 'InfValorBPe';
        OpenDataSource := False;
        DataSet        := cdsInfValorBPe;
    end;

    frxInfBPe := TfrxDBDataset.Create(Self);
    with frxInfBPe do
    begin
        UserName       := 'InfBPe';
        OpenDataSource := False;
        DataSet        := cdsInfBPe;
    end;

    frxIde := TfrxDBDataset.Create(Self);
    with frxIde do
    begin
        UserName       := 'Ide';
        OpenDataSource := False;
        DataSet        := cdsIde;
    end;

    frxPag := TfrxDBDataset.Create(Self);
    with frxPag do
    begin
        UserName       := 'Pag';
        OpenDataSource := False;
        DataSet        := cdsPag;
    end;

    frxProcBPe := TfrxDBDataset.Create(Self);
    with frxProcBPe do
    begin
        UserName       := 'ProcBPe';
        OpenDataSource := False;
        DataSet        := cdsProcBPe;
    end;

    frxInfAdic := TfrxDBDataset.Create(Self);
    with frxInfAdic do
    begin
        UserName       := 'InfAdic';
        OpenDataSource := False;
        DataSet        := cdsInfAdic;
    end;

    frxImp := TfrxDBDataset.Create(Self);
    with frxImp do
    begin
        UserName       := 'Imp';
        OpenDataSource := False;
        DataSet        := cdsImp;
    end;

    //By Sandro
    frxInfBPeSupl := TfrxDBDataset.Create(Self);
    with frxInfBPeSupl do
    begin
      UserName       := 'InfBPeSupl';
      OpenDataSource := False;
      DataSet        := cdsInfBPeSupl;
    end;


end;

destructor TACBrBPeDABPEFR.Destroy;
begin
  cdsEmitente.Free;
  cdsParametros.Free;
  cdsInfPassagem.Free;
  cdsInfViagem.Free;
  cdsAgencia.Free;
  cdsInfValorBPe.Free;
  cdsInfBPe.Free;
  cdsIde.Free;
  cdsPag.Free;
  cdsProcBPe.Free;
  cdsInfAdic.Free;
  cdsImp.Free;
  cdsInfBPeSupl.Free;

  inherited Destroy;
end;

procedure TACBrBPeDABPEFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
    qrcode: String;
    CpTituloReport, CpLogomarca, CpDescrProtocolo, CpTotTrib, CpContingencia1, CpContingencia2 : TfrxComponent;
begin
    qrCode := '';

    if Assigned(FBPe) then
    begin
        CpTituloReport := frxReport.FindObject('ReportTitle1');
        if Assigned(CpTituloReport) then
            CpTituloReport.Visible := cdsParametros.FieldByName('Imagem').AsString <> '';

        CpLogomarca := frxReport.FindObject('ImgLogo');
        if Assigned(CpLogomarca) and Assigned(CpTituloReport) then
            CpLogomarca.Visible := CpTituloReport.Visible;

        if EstaVazio(Trim(FBPe.infBPeSupl.qrCodBPe)) then
        begin
            qrcode := TACBrBPe(DABPEClassOwner.ACBrBPe).GetURLQRCode(FBPe.ide.cUF,
                                                                     FBPe.ide.tpAmb,
                                                                     OnlyNumber(FBPe.infBPe.ID))
        end
        else
            qrcode := FBPe.infBPeSupl.qrCodBPe;

        if Assigned(Sender) and (Sender.Name = 'ImgQrCode') then
            PintarQRCode(qrCode, TfrxPictureView(Sender).Picture.Bitmap, qrUTF8NoBOM);
    end;
end;

procedure TACBrBPeDABPEFR.frxReportGetValue(const VarName: string; var Value: Variant);
begin
  if VarName = 'CANCELADO' then
      Value := (DABPEClassOwner.Cancelada) or (FBPe.procBPe.cStat = 101);
end;

function TACBrBPeDABPEFR.GetPreparedReport: TfrxReport;
begin
  if Trim(FFastFile) = '' then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := frxReport
    else
      Result := nil;
  end;
end;

procedure TACBrBPeDABPEFR.ImprimirDABPE(BPe: TBPe);
begin
  if PrepareReport(BPe) then
  begin
    if MostraPreview then
      frxReport.ShowPreparedReport
    else
    begin
      frxReport.PrintOptions.ShowDialog  := SelecionaImpressora;
      frxReport.PrintOptions.Copies      := NumCopias;
      frxReport.PreviewOptions.AllowEdit := False;
      frxReport.Print;
    end;
  end;
end;

procedure TACBrBPeDABPEFR.ImprimirDABPEPDF(BPe: TBPe);
const
  TITULO_PDF = 'Bilhete de Passagem Eletrônico';
var
  OldShowDialog: Boolean;
begin
  if PrepareReport(BPe) then
  begin
    frxPDFExport.Author   := Sistema;
    frxPDFExport.Creator  := Sistema;
    frxPDFExport.Producer := Sistema;
    frxPDFExport.Title    := TITULO_PDF;
    frxPDFExport.Subject  := TITULO_PDF;
    frxPDFExport.Keywords := TITULO_PDF;

    OldShowDialog         := frxPDFExport.ShowDialog;

    try
      frxPDFExport.ShowDialog := False;
      frxPDFExport.FileName   := IncludeTrailingPathDelimiter(PathPDF) + OnlyNumber(FBPe.infBPe.ID) + '-bpe.pdf';

      if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
         ForceDirectories(ExtractFileDir(frxPDFExport.FileName));
      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := OldShowDialog;
    end;
  end;
end;

procedure TACBrBPeDABPEFR.LimpaDados;
begin
  cdsEmitente.EmptyDataSet;
  cdsParametros.EmptyDataSet;
  cdsInfPassagem.EmptyDataSet;
  cdsInfViagem.EmptyDataSet;
  cdsAgencia.EmptyDataSet;
  cdsInfValorBPe.EmptyDataSet;
  cdsInfBPe.EmptyDataSet;
  cdsIde.EmptyDataSet;
  cdsPag.EmptyDataSet;
  cdsProcBPe.EmptyDataSet;
  cdsInfAdic.EmptyDataSet;
  cdsImp.EmptyDataSet;
  cdsInfBPeSupl.EmptyDataSet;
end;

function TACBrBPeDABPEFR.PrepareReport(BPe: TBPe): Boolean;
var
    i: Integer;
    Stream: TStringStream;
begin
    Result := False;
    SetDataSetsToFrxReport;

    if Trim(FastFile) <> '' then
    begin
        if not (UpperCase(Copy(FastFile, Length(FastFile)-3, 4)) = '.FR3') then
        begin
            Stream := TStringStream.Create(FastFile);
            frxReport.FileName := '';
            frxReport.LoadFromStream(Stream);
            Stream.Free;
        end
        else
        begin
            if FileExists(FastFile) then
                frxReport.LoadFromFile(FastFile)
            else
                raise EACBrBPeDABPEFR.CreateFmt('Caminho do arquivo de impressão do DABPe "%s" inválido.', [FastFile]);
        end;
    end
    else
        raise EACBrBPeDABPEFR.Create('Caminho do arquivo de impressão do DABPe não assinalado.');

    if Assigned(BPe) then
    begin
        FBPe := BPe;
        CarregaDados;
        SetDataSetsToFrxReport;
        Result := frxReport.PrepareReport;
    end
    else
    begin
        if Assigned(ACBrBPe) then
        begin
            for i := 0 to TACBrBPe(ACBrBPe).Bilhetes.Count - 1 do
            begin
                FBPe := TACBrBPe(ACBrBPe).Bilhetes.Items[i].BPe;
                CarregaDados;
                if (i > 0) then
                    Result := frxReport.PrepareReport(False)
                else
                    Result := frxReport.PrepareReport;
            end;
        end
        else
            raise EACBrBPeDABPEFR.Create('Propriedade ACBrBPe não assinalada.');
    end;
end;

procedure TACBrBPeDABPEFR.SetDataSetsToFrxReport;
begin
  frxReport.DataSets.Clear;
  with frxReport.EnabledDataSets do
  begin
    Clear;
    Add(frxEmitente);
    Add(frxParametros);
    Add(frxInfPassagem);
    Add(frxInfViagem);
    Add(frxAgencia);
    Add(frxInfValorBPe);
    Add(frxInfBPe);
    Add(frxIde);
    Add(frxPag);
    Add(frxProcBPe);
    Add(frxInfAdic);
    Add(frxImp);
    Add(frxInfBPeSupl);
  end;
end;

procedure TACBrBPeDABPEFR.CarregaInfBpeSupl;
var
  s:string;
begin
  with cdsInfBPeSupl do
  begin
    Append;
    with FBPe.infBPeSupl do
    begin
      FieldByName('qrCodBPe').AsString := qrCodBPe;
      FieldByName('boardPassBPe').AsString := boardPassBPe;
    end;
    Post;
  end;
end;

end.
