{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{ Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimento}
{ Transporte eletrônica - CTe - http://www.cte.fazenda.gov.br                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
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

{******************************************************************************
|* Historico
|*
|* 30/03/2011: Jeickson Gobeti
|*  - Inicio do desenvolvimento Dacte FastReport
******************************************************************************}
{$I ACBr.inc}

unit ACBrCTeDACTEFRDM;

interface

uses
  SysUtils, Classes, Dialogs, ACBrCTeDACTEClass, pcteCTe, frxClass, frxExportPDF, DB,
  DBClient, frxDBSet, pcnConversao, frxBarcode, MaskUtils, pcnCadEmiDFe, pcteEnvEventoCTe;

type
  TdmACBrCTeFR = class(TDataModule)
    frxPDFExport: TfrxPDFExport;
    cdsIdentificacao: TClientDataSet;
    cdsEmitente: TClientDataSet;
    cdsDestinatario: TClientDataSet;
    cdsDadosNotasFiscais: TClientDataSet;
    cdsParametros: TClientDataSet;
    cdsInformacoesAdicionais: TClientDataSet;
    cdsVolumes: TClientDataSet;
    frxIdentificacao: TfrxDBDataset;
    frxEmitente: TfrxDBDataset;
    frxDestinatario: TfrxDBDataset;
    frxDadosNotasFiscais: TfrxDBDataset;
    frxParametros: TfrxDBDataset;
    frxVolumes: TfrxDBDataset;
    frxInformacoesAdicionais: TfrxDBDataset;
    frxBarCodeObject: TfrxBarCodeObject;
    frxReport: TfrxReport;
    cdsTomador: TClientDataSet;
    frxTomador: TfrxDBDataset;
    cdsExpedidor: TClientDataSet;
    frxExpedidor: TfrxDBDataset;
    cdsRecebedor: TClientDataSet;
    frxRecebedor: TfrxDBDataset;
    cdsRemetente: TClientDataSet;
    frxRemetente: TfrxDBDataset;
    cdsCalculoImposto: TClientDataSet;
    frxCalculoImposto: TfrxDBDataset;
    cdsComponentesPrestacao: TClientDataSet;
    frxComponentesPrestacao: TfrxDBDataset;
    cdsSeguro: TClientDataSet;
    frxSeguro: TfrxDBDataset;
    cdsModalRodoviario: TClientDataSet;
    frxModalRodoviario: TfrxDBDataset;
    cdsRodoVeiculos: TClientDataSet;
    frxRodoVeiculos: TfrxDBDataset;
    frxRodoValePedagio: TfrxDBDataset;
    cdsRodoValePedagio: TClientDataSet;
    cdsRodoMotorista: TClientDataSet;
    frxRodoMotorista: TfrxDBDataset;
    frxDocAnterior: TfrxDBDataset;
    cdsDocAnterior: TClientDataSet;
    cdsAnuladoComple: TClientDataSet;
    frxcdsAnuladoComple: TfrxDBDataset;
    cdsEventos: TClientDataSet;
    frxEventos: TfrxDBDataset;
    constructor Create(AOwner: TComponent); override;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
  private
    { Private declarations }
    FDACTEClassOwner: TACBrCTeDACTEClass;
    FCTe: TCTe;
    FEvento: TEventoCTe;
    procedure CarregaIdentificacao;
    procedure CarregaTomador;
    procedure CarregaEmitente;
    procedure CarregaRemetente;
    procedure CarregaDestinatario;
    procedure CarregaExpedidor;
    procedure CarregaRecebedor;
    procedure CarregaDadosNotasFiscais;
    procedure CarregaCalculoImposto;
    procedure CarregaParametros;
    procedure CarregaVolumes;
    procedure CarregaComponentesPrestacao;
    procedure CarregaSeguro;
    procedure CarregaModalRodoviario;
    procedure CarregaInformacoesAdicionais;
    procedure CarregaDocumentoAnterior; // Adicionado por NCC - 04/04/2014
    procedure CarregaCTeAnuladoComplementado; // Adicionado por NCC - 24/04/2014
  public
    { Public declarations }
    property CTe: TCTe read FCTe write FCTe;
    property Evento          : TEventoCTe read FEvento write FEvento;
    property DACTEClassOwner: TACBrCTeDACTEClass read FDACTEClassOwner;
    procedure CarregaDados;
    procedure CarregaDadosEventos;
  end;

var
  dmACBrCTeFR: TdmACBrCTeFR;
  TipoEvento: TpcnTpEvento;

implementation

uses ACBrCTe, ACBrCTeUtil, ACBrDFeUtil, StrUtils, Math;

{$R *.dfm}

type
  ArrOfStr = array of string;
  TSplitResult = array of string;

  { TdmACBrNFeFR }

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

function Explode(sPart, sInput: string): ArrOfStr;
begin
  while Pos(sPart, sInput) <> 0 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := Copy(sInput, 0, Pos(sPart, sInput) - 1);
    Delete(sInput, 1, Pos(sPart, sInput));
  end;

  SetLength(Result, Length(Result) + 1);
  Result[Length(Result) - 1] := sInput;
end;

function CollateBr(Str: string): string;
var
  Resultado, Temp: string;
  vChar: Char;
  Tamanho, i: integer;
begin
  Result := '';
  Tamanho := Length(str);
  i := 1;
  while i <= Tamanho do
  begin
    Temp := Copy(str, i, 1);
    vChar := Temp[1];
    case vChar of
      'á', 'â', 'ã', 'à', 'ä', 'å', 'Á', 'Â', 'Ã', 'À', 'Ä', 'Å': Resultado := 'A';
      'é', 'ê', 'è', 'ë', 'É', 'Ê', 'È', 'Ë': Resultado := 'E';
      'í', 'î', 'ì', 'ï', 'Í', 'Î', 'Ì', 'Ï': Resultado := 'I';
      'ó', 'ô', 'õ', 'ò', 'ö', 'Ó', 'Ô', 'Õ', 'Ò', 'Ö': Resultado := 'O';
      'ú', 'û', 'ù', 'ü', 'Ú', 'Û', 'Ù', 'Ü': Resultado := 'U';
      'ç', 'Ç': Resultado := 'C';
      'ñ', 'Ñ': Resultado := 'N';
      'ý', 'ÿ', 'Ý', 'Y': Resultado := 'Y';
      else
        if vChar > #127 then Resultado := #32
        {$IFDEF DELPHI12_UP}
        else if CharInset(vChar, ['a'..'z', 'A'..'Z', '0'..'9', '-', ' ']) then
        {$ELSE}
        else if vChar in ['a'..'z', 'A'..'Z', '0'..'9', '-', ' '] then
        {$ENDIF}
          Resultado := UpperCase(vCHAR);
    end;
    Result := Result + Resultado;
    i := i + 1;
  end;
end;

procedure TdmACBrCTeFR.CarregaCalculoImposto;
begin
  with cdsCalculoImposto do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('TXTSITTRIB', ftString, 60);
    FieldDefs.Add('VBC', ftFloat);
    FieldDefs.Add('PICMS', ftFloat);
    FieldDefs.Add('VICMS', ftFloat);
    FieldDefs.Add('pRedBC', ftFloat);
    FieldDefs.Add('VICMSST', ftFloat);
    FieldDefs.Add('VCREDITO', ftFloat);
    FieldDefs.Add('vIndSN', ftInteger);

    CreateDataSet;
    Append;

{$IFDEF PL_103}
    case FCTe.Imp.ICMS.SituTrib of
      cst00:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst00) + '-' + CSTICMSToStrTagPosText(cst00);
          FieldByName('vBC').AsFloat := FCTe.Imp.ICMS.CST00.vBC;
          FieldByName('pICMS').AsFloat := FCTe.Imp.ICMS.CST00.pICMS;
          FieldByName('vICMS').AsFloat := FCTe.Imp.ICMS.CST00.VICMS;
        end;
      cst45:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst45) + '-' + CSTICMSToStrTagPosText(cst45);
        end;
    end;
//{$ENDIF}
//{$IFDEF PL_104}
{$ELSE}
    case FCTe.Imp.ICMS.SituTrib of
      cst00:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst00) + '-' + CSTICMSToStrTagPosText(cst00);
          FieldByName('vBC').AsFloat := FCTe.Imp.ICMS.ICMS00.vBC;
          FieldByName('pICMS').AsFloat := FCTe.Imp.ICMS.ICMS00.pICMS;
          FieldByName('vICMS').AsFloat := FCTe.Imp.ICMS.ICMS00.VICMS;
        end;
      cst20:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst20) + '-' + CSTICMSToStrTagPosText(cst20);
          FieldByName('pRedBC').AsFloat := FCTe.Imp.ICMS.ICMS20.pRedBC;
          FieldByName('vBC').AsFloat := FCTe.Imp.ICMS.ICMS20.vBC;
          FieldByName('pICMS').AsFloat := FCTe.Imp.ICMS.ICMS20.pICMS;
          FieldByName('vICMS').AsFloat := FCTe.Imp.ICMS.ICMS20.VICMS;
        end;
      { Alterado por Jose Nilton Pace em 16/05/2013 }
      cst40:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst40) + '-' + CSTICMSToStrTagPosText(cst40);
        end;
      { Alterado por Jose Nilton Pace em 16/05/2013 }
      cst41:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst41) + '-' + CSTICMSToStrTagPosText(cst41);
        end;

      cst45:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst45) + '-' + CSTICMSToStrTagPosText(cst45);
        end;

      { Alterado por Jose Nilton Pace em 16/05/2013 }
      cst51:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst51) + '-' + CSTICMSToStrTagPosText(cst51);
        end;


      cst60:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst60) + '-' + CSTICMSToStrTagPosText(cst60);
          FieldByName('vBC').AsFloat := FCTe.Imp.ICMS.ICMS60.vBCSTRet;
          FieldByName('pICMS').AsFloat := FCTe.Imp.ICMS.ICMS60.pICMSSTRet;
          FieldByName('vICMS').AsFloat := FCTe.Imp.ICMS.ICMS60.vICMSSTRet;
          FieldByName('vCredito').AsFloat := FCTe.Imp.ICMS.ICMS60.vCred;
        end;
      cst90:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cst90) + '-' + CSTICMSToStrTagPosText(cst90);
          FieldByName('pRedBC').AsFloat := FCTe.Imp.ICMS.ICMS90.pRedBC;
          FieldByName('vBC').AsFloat := FCTe.Imp.ICMS.ICMS90.vBC;
          // Alterado por Italo em 22/10/2012  
          FieldByName('pICMS').AsFloat := FCTe.Imp.ICMS.ICMS90.pICMS;
          FieldByName('vICMS').AsFloat := FCTe.Imp.ICMS.ICMS90.vICMS;
          FieldByName('vCredito').AsFloat := FCTe.Imp.ICMS.ICMS90.vCred;
        end;
      // Incluido por Italo em 05/12/2011 (contribuição de Doni Dephi)
      cstICMSOutraUF:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cstICMSOutraUF) + '-' + CSTICMSToStrTagPosText(cstICMSOutraUF);
          FieldByName('pRedBC').AsFloat := FCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF;
          FieldByName('vBC').AsFloat := FCTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF;
          FieldByName('pICMS').AsFloat := FCTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF;// pRedBCOutraUF;
          FieldByName('vICMS').AsFloat := FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF;
        end;
      cstICMSSN:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStr(cstICMSSN) + '-' + CSTICMSToStrTagPosText(cstICMSSN);
          FieldByName('vIndSN').AsFloat := FCTe.Imp.ICMS.ICMSSN.indSN;
        end;
    end;
{$ENDIF}
    Post;
  end;
end;

procedure TdmACBrCTeFR.CarregaComponentesPrestacao;
var
  I: Integer;
begin
  with cdsComponentesPrestacao do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('Nome', ftString, 60);
    FieldDefs.Add('Valor', ftFloat);
    FieldDefs.Add('TotalServico', ftFloat);
    FieldDefs.Add('TotalReceber', ftFloat);
    CreateDataSet;

    if CTe.vPrest.comp.Count > 0 then
    begin
      for I := 0 to CTe.vPrest.comp.Count - 1 do
      begin
        Append;
        FieldByName('Nome').AsString := CTe.vPrest.comp.Items[I].xNome;
        FieldByName('Valor').AsFloat := CTe.vPrest.comp.Items[I].vComp;
        FieldByName('TotalServico').AsFloat := CTe.vPrest.vTPrest;
        FieldByName('TotalReceber').AsFloat := CTe.vPrest.vRec;
        Post;
      end;
    end
    else
    begin
      Append;
      FieldByName('Nome').AsString := '';
      FieldByName('Valor').AsFloat := 0;
      { Alterado por Jose Nilton Pace em 16/05/2013 }
//      FieldByName('TotalServico').AsFloat := 0;
//      FieldByName('TotalReceber').AsFloat := 0;
      FieldByName('TotalServico').AsFloat  := CTe.vPrest.vTPrest;
      FieldByName('TotalReceber').AsFloat  := CTe.vPrest.vRec;
      Post;
    end;
  end;

end;

procedure TdmACBrCTeFR.CarregaCTeAnuladoComplementado;
var i:integer;
begin
  with cdsAnuladoComple do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('Chave', ftString, 44);
    CreateDataSet;
    Append;
    {$IFDEF PL_200}
    if CTe.ide.tpCTe=tcComplemento then
      FieldbyName('Chave').AsString:=CTe.infCteComp.chave
    else if CTe.ide.tpCTe=tcAnulacao then
      FieldbyName('Chave').AsString:=CTe.infCteAnu.chCTe;
    {$ELSE}
    if CTe.ide.tpCTe=tcComplemento then
    begin
      if CTe.InfCTeComp.Count>=1 then
        FieldbyName('Chave').AsString:=CTe.InfCTeComp.Items[0].Chave;
    end else
    if CTe.ide.tpCTe=tcAnulacao then
      FieldbyName('Chave').AsString:=CTe.InfCTeAnuEnt.chCTe;
    {$ENDIF}

    Post;
  end;

end;

procedure TdmACBrCTeFR.CarregaDados;
begin
  CarregaIdentificacao;
  CarregaTomador;
  CarregaEmitente;
  CarregaRemetente;
  CarregaDestinatario;
  CarregaExpedidor;
  CarregaRecebedor;
  CarregaDadosNotasFiscais;
  CarregaParametros;
  CarregaCalculoImposto;
  CarregaVolumes;
  CarregaComponentesPrestacao;
  CarregaInformacoesAdicionais;
  CarregaSeguro;
  CarregaModalRodoviario;

  CarregaDocumentoAnterior;
  CarregaCTeAnuladoComplementado;

end;

procedure TdmACBrCTeFR.CarregaDadosEventos;
var
  i: integer;
  J: integer;
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
    Add('grupoAlterado', ftBlob);
    Add('campoAlterado', ftBlob);
    Add('valorAlterado', ftBlob);
    Add('nroItemAlterado', ftInteger);

    CreateDataSet;

    for i := 0 to FEvento.Evento.Count - 1 do
    begin
      with Evento.Evento[i] do
      begin
        case Evento.Evento[i].InfEvento.tpEvento of
          teCancelamento:
          begin
            TipoEvento:= teCancelamento;
            Append;
            FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);
            FieldByName('Modelo').AsString      := Copy(InfEvento.chCTe, 21, 2);
            FieldByName('Serie').AsString       := Copy(InfEvento.chCTe, 23, 3);
            FieldByName('Numero').AsString      := Copy(InfEvento.chCTe, 26, 9);
            FieldByName('MesAno').AsString      := Copy(InfEvento.chCTe, 05, 2) + '/' + Copy(InfEvento.chCTe, 03, 2);
            FieldByName('Barras').AsString      := InfEvento.chCTe;
            FieldByName('ChaveAcesso').AsString := CTEUtil.FormatarChaveAcesso(InfEvento.chCTe);
            FieldByName('cOrgao').AsInteger     := InfEvento.cOrgao;
            FieldByName('nSeqEvento').AsInteger := InfEvento.nSeqEvento;

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
            FieldByName('versaoEvento').AsString  := InfEvento.versaoEvento;
            FieldByName('cStat').AsInteger        := RetInfEvento.cStat;
            FieldByName('xMotivo').AsString       := RetInfEvento.xMotivo;
            FieldByName('nProt').AsString         := RetInfEvento.nProt;
            FieldByName('dhRegEvento').AsDateTime := RetInfEvento.dhRegEvento;
            FieldByName('xJust').AsString         := InfEvento.detEvento.xJust;
            FieldByName('xCondUso').AsString      := InfEvento.detEvento.xCondUso;

            Post;
          end;
          teCCe:
          begin
            TipoEvento:= teCCe;
            for J := 0 to InfEvento.detEvento.infCorrecao.Count -1 do
            begin
              Append;
              FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);
              FieldByName('Modelo').AsString      := Copy(InfEvento.chCTe, 21, 2);
              FieldByName('Serie').AsString       := Copy(InfEvento.chCTe, 23, 3);
              FieldByName('Numero').AsString      := Copy(InfEvento.chCTe, 26, 9);
              FieldByName('MesAno').AsString      := Copy(InfEvento.chCTe, 05, 2) + '/' + Copy(InfEvento.chCTe, 03, 2);
              FieldByName('Barras').AsString      := InfEvento.chCTe;
              FieldByName('ChaveAcesso').AsString := CTEUtil.FormatarChaveAcesso(InfEvento.chCTe);
              FieldByName('cOrgao').AsInteger     := InfEvento.cOrgao;
              FieldByName('nSeqEvento').AsInteger := InfEvento.nSeqEvento;

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
              FieldByName('versaoEvento').AsString  := InfEvento.versaoEvento;
              FieldByName('cStat').AsInteger        := RetInfEvento.cStat;
              FieldByName('xMotivo').AsString       := RetInfEvento.xMotivo;
              FieldByName('nProt').AsString         := RetInfEvento.nProt;
              FieldByName('dhRegEvento').AsDateTime := RetInfEvento.dhRegEvento;
              FieldByName('xJust').AsString         := InfEvento.detEvento.xJust;
              FieldByName('xCondUso').AsString      := InfEvento.detEvento.xCondUso;

              with InfEvento.detEvento.infCorrecao.Items[J] do
              begin
                FieldByName('grupoAlterado').AsString := grupoAlterado;
                FieldByName('campoAlterado').AsString := campoAlterado;
                FieldByName('valorAlterado').AsString := valorAlterado;
                FieldByName('nroItemAlterado').AsInteger := nroItemAlterado;
              end;

              Post;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TdmACBrCTeFR.CarregaDadosNotasFiscais;
var
  i: Integer;
  DoctoRem: string;
  NroNota: integer; // Adicionado por Rodrigo Cardilo
begin
  { dados das Notas Fiscais }
  DoctoRem := FCTe.Rem.CNPJCPF;
  if Length(DoctoRem) > 11 then
    DoctoRem := FormatMaskText('##.###.###\/####-##;0;_', DoctoRem)
  else
    DoctoRem := FormatMaskText('###.###.###-##;0;_', DoctoRem);

  with cdsDadosNotasFiscais do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('tpDoc', ftString, 5); //Tipo Documento
    FieldDefs.Add('CNPJCPF', ftString, 18); //CNPJCPF
    FieldDefs.Add('Serie', ftString, 3); // Serie
    FieldDefs.Add('ChaveAcesso', ftString, 44); // Chave Acesso
    FieldDefs.Add('NotaFiscal', ftString, 9); // Numero Nota Fiscal
    FieldDefs.Add('TextoImpressao', ftString, 100); // Texto Impressao no Relatorio
    CreateDataSet;

    {$IFDEF PL_200}
    for i := 0 to CTe.infCTeNorm.infDoc.infNF.Count - 1 do
    begin
      with FCTe.infCTeNorm.infDoc.InfNF.Items[i] do
    {$ELSE}
    for i := 0 to CTe.Rem.InfNF.Count - 1 do
    begin
      with FCTe.Rem.InfNF.Items[i] do
    {$ENDIF}
      begin
        Append;
        FieldByName('tpDoc').AsString := 'NF';
        FieldByName('CNPJCPF').AsString := FCTe.Rem.CNPJCPF;
        FieldByName('Serie').AsString := serie;
        FieldByName('ChaveAcesso').AsString := '';
        FieldByName('NotaFiscal').AsString := nDoc;
        { Alterado por Jose Nilton Pace em 16/05/2013 }
        FieldByName('TextoImpressao').AsString := 'NF                  ' + DoctoRem + '                                        '+
          serie+                                '  /  ' + FormatFloat('000000000', StrToInt(nDoc));
(*
        FieldByName('TextoImpressao').AsString := 'NF                 ' + DoctoRem + '                                      ' +
          FormatFloat('000', StrToInt(serie)) + '  /  ' + FormatFloat('000000000', StrToInt(nDoc));
*)
      end;
      Post;
    end;

    {$IFDEF PL_200}
    for i := 0 to CTe.infCTeNorm.infDoc.InfNFE.Count - 1 do
    begin
      with FCTe.infCTeNorm.infDoc.InfNFE.Items[i] do
    {$ELSE}
    for i := 0 to CTe.Rem.InfNFE.Count - 1 do
    begin
      with FCTe.Rem.InfNFE.Items[i] do
    {$ENDIF}
      begin
        Append;
        FieldByName('tpDoc').AsString := 'NFe';
        FieldByName('CNPJCPF').AsString := FCTe.Rem.CNPJCPF;
        FieldByName('Serie').AsString := copy(chave,23,3);
        FieldByName('ChaveAcesso').AsString := chave;
        FieldByName('NotaFiscal').AsString := copy(chave,26,9);
        NroNota := StrToInt(Copy(chave, 26, 9)); // Adicionado por Rodrigo Cardilo em 11/08/2014

        { Alterado por Jose Nilton Pace em 16/05/2013 
        FieldByName('TextoImpressao').AsString := 'NF-e                '+chave; }

        { Alterado por Rodrigo Cardilo em 11/08/2014 }
        FieldByName('TextoImpressao').AsString := 'NF-e ' + FormatFloat('000000000', NroNota) + '      ' + chave ;
(*
        FieldByName('TextoImpressao').AsString := 'NF-e            ' + chave;
*)
      end;
      Post;
    end;

    { Alterado por Jose Nilton Pace em 16/05/2013 }
    {$IFDEF PL_200}
    for i := 0 to CTe.infCTeNorm.infDoc.infOutros.Count - 1 do
    begin
      with FCTe.infCTeNorm.infDoc.infOutros.Items[i] do
    {$ELSE}
    for i := 0 to CTe.Rem.infOutros.Count - 1 do
    begin
      with FCTe.Rem.infOutros.Items[i] do
    {$ENDIF}
      begin
        Append;
        FieldByName('tpDoc').AsString := 'Outros';
        FieldByName('CNPJCPF').AsString := FCTe.Rem.CNPJCPF;
        FieldByName('Serie').AsString := '';
        FieldByName('ChaveAcesso').AsString := '';
        FieldByName('NotaFiscal').AsString := '';

        case tpDoc of
          tdDeclaracao: FieldByName('TextoImpressao').AsString := 'Declaração          '+DoctoRem+'                                        '+nDoc;
          tdOutros: FieldByName('TextoImpressao').AsString := 'Outros              '+DoctoRem+'                                        '+nDoc;
          tdDutoviario: FieldByName('TextoImpressao').AsString := 'Dutoviário          '+DoctoRem+'                                        '+nDoc;
          else
            FieldByName('TextoImpressao').AsString := 'Não informado       '+DoctoRem+'                                        '+nDoc;
        end;
      end;
      Post;
    end;

    //
    cdsDadosNotasFiscais.RecordCount;
  end;

end;

procedure TdmACBrCTeFR.CarregaDestinatario;
begin
  { destinatário }
  with cdsDestinatario do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('CNPJCPF', ftString, 18);
    FieldDefs.Add('XNome', ftString, 60);
    FieldDefs.Add('XLgr', ftString, 60);
    FieldDefs.Add('Nro', ftString, 60);
    FieldDefs.Add('XCpl', ftString, 60);
    FieldDefs.Add('XBairro', ftString, 60);
    FieldDefs.Add('CMun', ftString, 7);
    FieldDefs.Add('XMun', ftString, 60);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('CEP', ftString, 9);
    FieldDefs.Add('CPais', ftString, 4);
    FieldDefs.Add('XPais', ftString, 60);
    FieldDefs.Add('Fone', ftString, 15);
    FieldDefs.Add('IE', ftString, 20);

    CreateDataSet;
    Append;

    with FCTe.Dest do
    begin
      FieldByName('CNPJCPF').AsString := FormatarCNPJCPF(CNPJCPF);
      FieldByName('XNome').AsString := XNome;
      with EnderDest do
      begin
        FieldByName('XLgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('CPais').AsString := IntToStr(CPais);
        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
    end;
    Post;
  end;
end;

procedure TdmACBrCTeFR.CarregaDocumentoAnterior;    // Adicionado por NCC - 04/04/2014
var i,ii,iii:integer;
begin
  with cdsDocAnterior do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('CNPJCPF', ftString, 18);
    FieldDefs.Add('xNome', ftString, 60);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('IE', ftString, 20);

    FieldDefs.Add('Tipo', ftString, 33);
    FieldDefs.Add('Serie', ftString, 3);
    FieldDefs.Add('nDoc', ftString, 20);
    FieldDefs.Add('dEmi', ftString, 10);
    FieldDefs.Add('Chave', ftString, 44);
    CreateDataSet;

  {$IFDEF PL_200}
    for i := 0 to CTe.infCTeNorm.docAnt.emiDocAnt.Count - 1 do
    begin
      with CTe.infCTeNorm.docAnt.emiDocAnt.Items[i] do
      begin
  {$ELSE}
    for i := 0 to CTe.infCTeNorm.emiDocAnt.Count - 1 do
    begin
      with CTe.infCTeNorm.emiDocAnt.Items[i] do
      begin
  {$ENDIF}
        for ii := 0 to idDocAnt.Count - 1 do
        begin
          for iii := 0 to idDocAnt.Items[ii].idDocAntPap.Count-1 do
          begin
            with idDocAnt.Items[ii].idDocAntPap.Items[iii] do
            begin
              Append;
              FieldByName('CNPJCPF').AsString:=CNPJCPF;
              FieldByName('xNome').AsString:=xNome;
              FieldByName('UF').AsString:=UF;
              case tpDoc of
                daCTRC: FieldByName('Tipo').AsString:='CTRC';
                daCTAC: FieldByName('Tipo').AsString:='CTAC';
                daACT: FieldByName('Tipo').AsString:='ACT';
                daNF7: FieldByName('Tipo').AsString:='NF 7';
                daNF27: FieldByName('Tipo').AsString:='NF 27';
                daCAN: FieldByName('Tipo').AsString:='CAN';
                daCTMC: FieldByName('Tipo').AsString:='CTMC';
                daATRE: FieldByName('Tipo').AsString:='ATRE';
                daDTA: FieldByName('Tipo').AsString:='DTA';
                daCAI: FieldByName('Tipo').AsString:='CAI';
                daCCPI: FieldByName('Tipo').AsString:='CCPI';
                daCA: FieldByName('Tipo').AsString:='CA';
                daTIF: FieldByName('Tipo').AsString:='TIF';
                daOutros: FieldByName('Tipo').AsString:='OUTROS';
              end;
//              FieldByName('Serie').AsString:=idDocAnt.Items[i].idDocAntPap.Items[ii].serie;
//              FieldByName('nDoc').AsString:=intToStr(idDocAnt.Items[i].idDocAntPap.Items[ii].nDoc);
//              FieldByName('dEmi').AsString:=FormatDateTime('dd/mm/yyyy',idDocAnt.Items[i].idDocAntPap.Items[ii].dEmi);
              FieldByName('Serie').AsString:=idDocAnt.Items[ii].idDocAntPap.Items[iii].serie;
              FieldByName('nDoc').AsString:=intToStr(idDocAnt.Items[ii].idDocAntPap.Items[iii].nDoc);
              FieldByName('dEmi').AsString:=FormatDateTime('dd/mm/yyyy',idDocAnt.Items[ii].idDocAntPap.Items[iii].dEmi);
            end;
            post;
          end;
          for iii := 0 to idDocAnt.Items[ii].idDocAntEle.Count-1 do
          begin
            Append;
            FieldByName('CNPJCPF').AsString:=CNPJCPF;
            FieldByName('xNome').AsString:=xNome;
            FieldByName('UF').AsString:=UF;
            with idDocAnt.Items[ii].idDocAntEle.Items[iii] do
            begin
              FieldByName('Tipo').AsString:='CT-e';
              FieldByName('Chave').AsString:=chave;
              FieldByName('Serie').AsString:=copy(chave,23,3);
              FieldByName('nDoc').AsString:=copy(chave,26,9);
              FieldByName('dEmi').AsString:=copy(chave,5,2)+'/'+copy(chave,3,2);
            end;
            post;
          end;
        end;
      end;
    end;
  end;
end;

procedure TdmACBrCTeFR.CarregaEmitente;
begin
  { emitente }
  with cdsEmitente do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('CNPJ', ftString, 18);
    FieldDefs.Add('XNome', ftString, 60);
    FieldDefs.Add('XFant', ftString, 60);
    FieldDefs.Add('XLgr', ftString, 60);
    FieldDefs.Add('Nro', ftString, 60);
    FieldDefs.Add('XCpl', ftString, 60);
    FieldDefs.Add('XBairro', ftString, 60);
    FieldDefs.Add('CMun', ftString, 7);
    FieldDefs.Add('XMun', ftString, 60);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('CEP', ftString, 9);
    FieldDefs.Add('CPais', ftString, 4);
    FieldDefs.Add('XPais', ftString, 60);
    FieldDefs.Add('Fone', ftString, 15);
    FieldDefs.Add('IE', ftString, 20);
    FieldDefs.Add('IM', ftString, 15);
    FieldDefs.Add('IEST', ftString, 20);
    FieldDefs.Add('CRT', ftString, 1);

    CreateDataSet;
    Append;

    with FCTE.Emit do
    begin
      FieldByName('CNPJ').AsString := FormatarCNPJCPF(CNPJ);
      FieldByName('XNome').AsString := XNome;
      FieldByName('XFant').AsString := XFant;
      with EnderEmit do
      begin
        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(CEP, 8));
        //        FieldByName('CPais').AsString := IntToStr(CPais);
        //        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
    end;

    Post;
  end;
end;

procedure TdmACBrCTeFR.CarregaExpedidor;
begin
  { Expedidor }
  with cdsExpedidor do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('CNPJ', ftString, 18);
    FieldDefs.Add('XNome', ftString, 60);
    FieldDefs.Add('XFant', ftString, 60);
    FieldDefs.Add('XLgr', ftString, 60);
    FieldDefs.Add('Nro', ftString, 60);
    FieldDefs.Add('XCpl', ftString, 60);
    FieldDefs.Add('XBairro', ftString, 60);
    FieldDefs.Add('CMun', ftString, 7);
    FieldDefs.Add('XMun', ftString, 60);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('CEP', ftString, 9);
    FieldDefs.Add('CPais', ftString, 4);
    FieldDefs.Add('XPais', ftString, 60);
    FieldDefs.Add('Fone', ftString, 15);
    FieldDefs.Add('IE', ftString, 20);
    FieldDefs.Add('IM', ftString, 15);
    FieldDefs.Add('IEST', ftString, 20);
    FieldDefs.Add('CRT', ftString, 1);

    CreateDataSet;
    Append;

    with FCTE.Exped do
    begin
      FieldByName('CNPJ').AsString := FormatarCNPJCPF(CNPJCPF);
      FieldByName('XNome').AsString := XNome;
      with EnderExped do
      begin
        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('CPais').AsString := IntToStr(CPais);
        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
    end;

    Post;
  end;

end;

procedure TdmACBrCTeFR.CarregaIdentificacao;
begin
  with cdsIdentificacao do
  begin
    Close;
    FieldDefs.Clear;
    //FieldDefs.Add('Versao', ftString, 4);
    FieldDefs.Add('Id', ftString, 44);
    FieldDefs.Add('Chave', ftString, 60);
    FieldDefs.Add('CUF', ftString, 2);
    FieldDefs.Add('CCT', ftString, 9);
    FieldDefs.Add('CFOP', ftString, 4);
    FieldDefs.Add('NatOp', ftString, 60);
    FieldDefs.Add('forPag', ftString, 50);
    FieldDefs.Add('Mod_', ftString, 2);
    FieldDefs.Add('Serie', ftString, 3);
    FieldDefs.Add('NCT', ftString, 9);
    FieldDefs.Add('dhEmi', ftDateTime);
    FieldDefs.Add('TpImp', ftString, 1);
    FieldDefs.Add('TpEmis', ftString, 50);
    FieldDefs.Add('CDV', ftString, 1);
    FieldDefs.Add('TpAmb', ftString, 1);
    FieldDefs.Add('TpCT', ftString, 50);
    FieldDefs.Add('ProcEmi', ftString, 1);
    FieldDefs.Add('VerProc', ftString, 20);
    FieldDefs.Add('cMunEmi', ftString, 7);
    FieldDefs.Add('xMunEmi', ftString, 60);
    FieldDefs.Add('UFEmi', ftString, 2);
    FieldDefs.Add('modal', ftString, 1);
    FieldDefs.Add('tpServ', ftString, 50);
    FieldDefs.Add('cMunIni', ftString, 7);
    FieldDefs.Add('xMunIni', ftString, 60);
    FieldDefs.Add('UFIni', ftString, 2);
    FieldDefs.Add('cMunFim', ftString, 7);
    FieldDefs.Add('xMunFim', ftString, 60);
    FieldDefs.Add('UFFim', ftString, 2);
    FieldDefs.Add('retira', ftString, 1);
    FieldDefs.Add('xDetRetira', ftString, 160);
    FieldDefs.Add('toma', ftString, 50);
    CreateDataSet;
    Append;

    with FCTe.infCTe do
    begin
      //FieldByName('Versao').AsString := IntToStr(Versao);
      FieldByName('Id').AsString := LimpaNumero(Id);
      FieldByName('Chave').AsString := CTeUtil.FormatarChaveAcesso(Id);
    end;

    with FCTe.Ide do
    begin
      FieldByName('CUF').AsString := IntToStr(CUF);
      FieldByName('CCT').AsString := IntToStr(CCT);
      FieldByName('CFOP').AsString := IntToStr(CFOP);
      FieldByName('NatOp').AsString := NatOp;

      case forPag of
        fpPago: FieldByName('forPag').AsString := 'Pago';
        fpAPagar: FieldByName('forPag').AsString := 'A Pagar';
        fpOutros: FieldByName('forPag').AsString := 'Outros';
      end;

      FieldByName('Mod_').AsString := modelo;
      FieldByName('Serie').AsString := IntToStr(Serie);
      FieldByName('NCT').AsString := CTeUtil.FormatarNumCTe(nCT);
      FieldByName('dhEmi').AsDateTime := dhEmi;

      case tpCTe of
        tcNormal: FieldByName('TpCT').AsString := 'Normal';
        tcComplemento: FieldByName('TpCT').AsString := 'Complemento';
        tcAnulacao: FieldByName('TpCT').AsString := 'Anulação';
        tcSubstituto: FieldByName('TpCT').AsString := 'Substituto';
      end;
{$IFDEF PL_103}
      FieldByName('cMunEmi').AsString := IntToStr(cMunEmi);
      FieldByName('xMunEmi').AsString := xMunEmi;
      FieldByName('UFEmi').AsString := UFEmi;
//{$ENDIF}
//{$IFDEF PL_104}
{$ELSE}
      FieldByName('cMunEmi').AsString := IntToStr(cMunEnv);
      FieldByName('xMunEmi').AsString := xMunEnv;
      FieldByName('UFEmi').AsString := UFEnv;
{$ENDIF}

      FieldByName('modal').AsString := SeSenao(modal = mdRodoviario, '0', '0');

      case tpServ of
        tsNormal: FieldByName('tpServ').AsString := 'Normal';
        tsSubcontratacao: FieldByName('tpServ').AsString := 'Subcontratação';
        tsRedespacho: FieldByName('tpServ').AsString := 'Redespacho';
        tsIntermediario: FieldByName('tpServ').AsString := 'Intermediário';
      end;

      FieldByName('cMunIni').AsString := IntToStr(cMunIni);
      FieldByName('xMunIni').AsString := xMunIni;
      FieldByName('UFIni').AsString := UFIni;
      FieldByName('cMunFim').AsString := IntToStr(cMunFim);
      FieldByName('xMunFim').AsString := xMunFim;
      FieldByName('UFFim').AsString := UFFim;
      FieldByName('TpImp').AsString := SeSenao(TpImp = tiRetrato, '1', '2');
      FieldByName('TpEmis').AsString := SeSenao(TpEmis = teNormal, '1', '5');
      FieldByName('CDV').AsString := IntToStr(CDV);
      FieldByName('TpAmb').AsString := SeSenao(TpAmb = taHomologacao, '2', '1');
      FieldByName('ProcEmi').AsString := SeSenao(ProcEmi = peAplicativoContribuinte, '0', '');
      FieldByName('VerProc').AsString := VerProc;

      case Toma03.Toma of
        tmRemetente: FieldByName('Toma').AsString := 'Remetente';
        tmDestinatario: FieldByName('Toma').AsString := 'Destinatário';
        tmExpedidor: FieldByName('Toma').AsString := 'Expedidor';
        tmRecebedor: FieldByName('Toma').AsString := 'Recebedor';
      end;

      case Toma4.Toma of
        tmOutros: FieldByName('Toma').AsString := 'Outros';
      end;
    end;
    Post;
  end;
end;

procedure TdmACBrCTeFR.CarregaInformacoesAdicionais;
var
  vTemp: TStringList;
  IndexCampo: Integer;
  Campos: TSplitResult;
  BufferObs: string;
  TmpStr: string;
  wContingencia: string;
  wObs: string;
  wSubstituto: string;
  i:integer;
begin

  with cdsInformacoesAdicionais do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('OBS', ftString, 2000);
    FieldDefs.Add('infAdFisco', ftString, 2000);
    FieldDefs.Add('ObsCont', ftString, 1800);
    FieldDefs.Add('Fluxo_xOrig', ftString, 15);
    FieldDefs.Add('Fluxo_xDest', ftString, 15);
    FieldDefs.Add('Fluxo_xRota', ftString, 15);

    CreateDataSet;
    Append;
    with FCTe.compl do
    begin
      wObs := xObs;

   {Adicionado por Rodrigo Cardilo em 11/08/2014}
   // Observações 
      {
      for i := 0 to ObsCont.Count - 1 do
      begin
        with ObsCont.Items[i] do
          TmpStr := TmpStr + XCampo + ': ' + XTexto + ';';
      end;

      if Length(wObs) > 0 then
        wObs := wObs + ';';
      wObs := wObs + TmpStr;

      TmpStr := '';
      }

      {$IFDEF PL_200}
      if CTe.ide.tpCTe=tcSubstituto then // Adicionado por NCC em 24/04/2014
      begin
        wSubstituto:='Chave do CT-e a ser substituido (Original): '+CTe.infCTeNorm.infCteSub.chCte+';';
        if length(CTe.infCTeNorm.infCteSub.tomaICMS.refNFe)>0 then
          wSubstituto:=wSubstituto+'Chave da NF-e emitida pelo tomador: '+CTe.infCTeNorm.infCteSub.tomaICMS.refNFe+';'
        else if length(CTe.infCTeNorm.infCteSub.tomaICMS.refCte)>0 then
          wSubstituto:=wSubstituto+'Chave do CT-e emitido pelo tomador: '+CTe.infCTeNorm.infCteSub.tomaICMS.refCte+';'
        else if CTe.infCTeNorm.infCteSub.tomaICMS.refNF.nro>0 then
          wSubstituto:=wSubstituto+'Número/Série da nota emitida pelo tomador: '+intToStr(CTe.infCTeNorm.infCteSub.tomaICMS.refNF.nro)+' / '+
                       intToStr(CTe.infCTeNorm.infCteSub.tomaICMS.refNF.serie)+';'
        else if length(CTe.infCTeNorm.infCteSub.tomaNaoICMS.refCteAnu)>0 then
          wSubstituto:=wSubstituto+'Chave do CT-e de anulação: '+CTe.infCTeNorm.infCteSub.tomaNaoICMS.refCteAnu+';';

        if Length(wObs) > 0 then
          wObs := wObs + ';';
        wObs := wObs + wSubstituto;

      end;
      {$ENDIF}

      //Contingencia
      if FCTe.Ide.tpEmis = teNORMAL then
        wContingencia := ''
      else
      begin
        if (FCTe.Ide.tpEmis = teContingencia) or (FCTe.Ide.tpEmis = teFSDA) or (FCTe.Ide.tpEmis = teSCAN) then
          wContingencia := 'DACTE EM CONTINGÊNCIA, IMPRESSO EM DECORRÊNCIA DE PROBLEMAS TÉCNICOS'
        else if FCTe.Ide.tpEmis = teDPEC then
          wContingencia := 'DACTE IMPRESSO EM CONTINGÊNCIA - DPEC REGULARMENTE RECEBIDA PELA RECEITA FEDERAL DO BRASIL';

        //            wContingencia := wContingencia + ';' +
        //            'DATA/HORA INÍCIO: ' + SeSenao(FCTe.ide.dhCont = 0, ' ', DateTimeToStr(FCTe.ide.dhCont)) + ';'+
        //            'MOTIVO CONTINGÊNCIA: ' + SeSenao(EstaVazio(FCTe.ide.xJust), ' ', FCTe.ide.xJust);
      end;
      if Length(wObs) > 0 then
        wObs := wObs + ';';
      wObs := wObs + wContingencia;

      vTemp := TStringList.Create;
      try
        if Trim(wObs) <> '' then
        begin
          Campos := Split(';', wObs);
          for IndexCampo := 0 to Length(Campos) - 1 do
            vTemp.Add(Campos[IndexCampo]);

          TmpStr := vTemp.Text;
          BufferObs := TmpStr;
        end
        else
          BufferObs := '';

      finally
        vTemp.Free;
      end;

      FieldByName('Fluxo_xOrig').AsString := fluxo.xOrig;
      FieldByName('Fluxo_xDest').AsString := fluxo.xDest;
      FieldByName('Fluxo_xRota').AsString := fluxo.xRota;

    end;
    FieldByName('OBS').AsString := BufferObs;

// adicionado por NCC em 22/04/14 - Início
    BufferObs:='';
    if trim(FCTe.imp.infAdFisco)<>'' then
    begin
      wObs:= FCTe.imp.infAdFisco;
      vTemp := TStringList.Create;
      try
        if Trim(wObs) <> '' then
        begin
          Campos := Split(';', wObs);
          for IndexCampo := 0 to Length(Campos) - 1 do
            vTemp.Add(Campos[IndexCampo]);

          TmpStr := vTemp.Text;
          BufferObs := TmpStr;
        end
        else
          BufferObs := '';

      finally
        vTemp.Free;
      end;
    end;
    FieldByName('infAdFisco').AsString := BufferObs;

    BufferObs:='';
    if FCTe.compl.ObsCont.Count>0 then
    begin
      wObs:='';
      for I := 0 to FCTe.compl.ObsCont.Count - 1 do
        wObs:= wObs + FCTe.compl.ObsCont[i].xCampo+' : '+FCTe.compl.ObsCont[i].xTexto+';';

      vTemp := TStringList.Create;
      try
        if Trim(wObs) <> '' then
        begin
          Campos := Split(';', wObs);
          for IndexCampo := 0 to Length(Campos) - 1 do
            vTemp.Add(Campos[IndexCampo]);

          TmpStr := vTemp.Text;
          BufferObs := TmpStr;
        end
        else
          BufferObs := '';

      finally
        vTemp.Free;
      end;

    end;
    FieldByName('ObsCont').AsString := BufferObs;
// adicionado por NCC em 22/04/14 - Fim

    Post;
  end;

end;

procedure TdmACBrCTeFR.CarregaModalRodoviario;
var
  i: integer;
begin
  with cdsModalRodoviario do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('RNTRC', ftString, 60);
    FieldDefs.Add('DATAPREVISTA', ftString, 60);
    FieldDefs.Add('LOTACAO', ftString, 60);
    FieldDefs.Add('CIOT', ftString, 12);
    FieldDefs.Add('LACRES', ftString, 255);
    CreateDataSet;
    Append;
    //
    {$IFDEF PL_200}
    case CTe.infCTeNorm.rodo.lota of
    {$ELSE}
    case CTe.Rodo.Lota of
    {$ENDIF}
      ltNao: FieldByName('LOTACAO').AsString := 'Não';
      ltSim: FieldByName('LOTACAO').AsString := 'Sim';
    end;

    {$IFDEF PL_200}
    with CTe.infCTeNorm.rodo do
    {$ELSE}
    with CTe.Rodo do
    {$ENDIF}
    begin
      FieldByName('RNTRC').AsString := RNTRC;
      if DateToStr(dPrev)<>'30/12/1899' then //alterado por NCC em 24/04/14
        FieldByName('DATAPREVISTA').AsString := DateToStr(dPrev);
      FieldByName('CIOT').AsString := CIOT;
    end;

    {$IFDEF PL_200}
    for I := 0 to CTe.infCTeNorm.rodo.lacRodo.Count - 1 do
    begin
      with CTe.infCTeNorm.rodo.lacRodo.Items[I] do
    {$ELSE}
    for I := 0 to CTe.Rodo.Lacres.Count - 1 do
    begin
      with CTe.Rodo.Lacres.Items[I] do
    {$ENDIF}
      begin
        if Trim(FieldByName('LACRES').AsString) <> '' then
          FieldByName('LACRES').AsString := FieldByName('LACRES').AsString + '/';
        FieldByName('LACRES').AsString := FieldByName('LACRES').AsString + nLacre;
      end;
    end;

    Post;
  end;

  with cdsRodoVeiculos do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('tpVeic', ftString, 10);
    FieldDefs.Add('placa', ftString, 7);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('RNTRC', ftString, 8);
    CreateDataSet;

    {$IFDEF PL_200}
    for i := 0 to CTe.infCTeNorm.rodo.veic.Count - 1 do
    begin
      with CTe.infCTeNorm.rodo.veic.Items[i] do
      begin
    {$ELSE}
    for i := 0 to CTe.Rodo.veic.Count - 1 do
    begin
      with CTe.Rodo.veic.Items[i] do
      begin
    {$ENDIF}
        Append;
        case tpVeic of
          tvTracao: FieldByName('tpVeic').AsString := 'Tração';
          tvReboque: FieldByName('tpVeic').AsString := 'Reboque';
        end;
        FieldByName('placa').AsString := placa;
        FieldByName('UF').AsString := UF;
        FieldByName('RNTRC').AsString := Prop.RNTRC;
        Post;
      end;
    end;
  end;

  with cdsRodoValePedagio do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('CNPJPg', ftString, 18);
    FieldDefs.Add('CNPJForn', ftString, 18);
    FieldDefs.Add('nCompra', ftString, 14);
    CreateDataSet;

    {$IFDEF PL_104}
    for i := 0 to CTe.Rodo.valePed.Count - 1 do
    begin
      Append;
      FieldByName('CNPJForn').AsString := FormatarCNPJCPF(CTe.Rodo.valePed.Items[i].CNPJForn);
      FieldByName('CNPJPg').AsString := FormatarCNPJCPF(CTe.Rodo.valePed.Items[i].CNPJPg);
      FieldByName('nCompra').AsString := CTe.Rodo.valePed.Items[i].nCompra;
      Post;
    end;
    {$ENDIF}
    {$IFDEF PL_200}
    for i := 0 to CTe.infCTeNorm.rodo.valePed.Count - 1 do
    begin
      Append;
      FieldByName('CNPJForn').AsString := FormatarCNPJCPF(CTe.infCTeNorm.rodo.valePed.Items[i].CNPJForn);
      FieldByName('CNPJPg').AsString := FormatarCNPJCPF(CTe.infCTeNorm.rodo.valePed.Items[i].CNPJPg);
      FieldByName('nCompra').AsString := CTe.infCTeNorm.rodo.valePed.Items[i].nCompra;
      Post;
    end;
    {$ENDIF}
  end;

  with cdsRodoMotorista do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('xNome', ftString, 60);
    FieldDefs.Add('CPF', ftString, 11);
    CreateDataSet;

    {$IFDEF PL_200}
    for i := 0 to CTe.infCTeNorm.rodo.moto.Count - 1 do
    begin
      with CTe.infCTeNorm.rodo.moto.Items[i] do
      begin
    {$ELSE}
    for i := 0 to CTe.Rodo.moto.Count - 1 do
    begin
      with CTe.Rodo.moto.Items[i] do
      begin
    {$ENDIF}
        Append;
        FieldByName('xNome').AsString := xNome;
        FieldByName('CPF').AsString := CPF;
        Post;
      end;
    end;
  end;
end;

procedure TdmACBrCTeFR.CarregaParametros;
var
  vChave_Contingencia: string;
  vResumo: string;
  vStream: TMemoryStream;
  vStringStream: TStringStream;
begin
  { parâmetros }
  with cdsParametros do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('ResumoCanhoto', ftString, 200);
    FieldDefs.Add('Mensagem0', ftString, 60);
    FieldDefs.Add('Versao', ftString, 5);
    FieldDefs.Add('Imagem', ftString, 256);
    FieldDefs.Add('Sistema', ftString, 60);
    FieldDefs.Add('Usuario', ftString, 60);
    FieldDefs.Add('Fax', ftString, 60);
    FieldDefs.Add('Site', ftString, 60);
    FieldDefs.Add('Email', ftString, 60);
    FieldDefs.Add('Desconto', ftString, 60);
    FieldDefs.Add('ChaveAcesso_Descricao', ftString, 90);
    FieldDefs.Add('Contingencia_ID', ftString, 36);
    FieldDefs.Add('Contingencia_Descricao', ftString, 60);
    FieldDefs.Add('Contingencia_Valor', ftString, 60);
    FieldDefs.Add('LinhasPorPagina', ftInteger);
    FieldDefs.Add('LogoCarregado', ftBlob);

    CreateDataSet;
    Append;

    vResumo := '';
    //    if DACTEClassOwner.ExibirResumoCanhoto then
    //    begin
    //       if NotaUtil.EstaVazio(DANFEClassOwner.ExibirResumoCanhoto_Texto) then
    //          vResumo := 'Emissão: ' + NotaUtil.FormatDate(DateToStr(FNFe.Ide.DEmi)) + '  Dest/Reme: ' + FNFe.Dest.XNome + '  Valor Total: ' + NotaUtil.FormatFloat(FNFe.Total.ICMSTot.VNF)
    //       else
    //          vResumo := DANFEClassOwner.ExibirResumoCanhoto_Texto;
    //    end;
    FieldByName('ResumoCanhoto').AsString := vResumo;
    {$IFDEF PL_103}
    FieldByName('Versao').AsString := '1.03';
    {$ENDIF}
    {$IFDEF PL_104}
    FieldByName('Versao').AsString := '1.04';
    {$ENDIF}
    {$IFDEF PL_200}
    FieldByName('Versao').AsString := '2.00';
    {$ENDIF}
    if (FCTe.Ide.TpAmb = taHomologacao) then
      FieldByName('Mensagem0').AsString := 'CTe sem Valor Fiscal - HOMOLOGAÇÃO'
    else
    begin
      if not (FCTe.Ide.tpEmis in [teContingencia, teFSDA]) then
      begin
        if ((EstaVazio(FDACTEClassOwner.ProtocoloCTE)) and
          (EstaVazio(FCTe.procCTe.nProt))) then
          FieldByName('Mensagem0').AsString := 'CTe sem Autorização de Uso da SEFAZ'
        else
          if (not ((EstaVazio(FDACTEClassOwner.ProtocoloCTE)) and
            (EstaVazio(FCTe.procCTe.nProt)))) and
            (FCTe.procCTe.cStat = 101) then
            FieldByName('Mensagem0').AsString := 'CTe Cancelado'
          else
          begin
            if FDACTEClassOwner.CTeCancelada then
              FieldByName('Mensagem0').AsString := 'CTe Cancelado'
            else
              FieldByName('Mensagem0').AsString := '';
          end;
      end
      else
        FieldByName('Mensagem0').AsString := '';
    end;

    // Carregamento da imagem
    if DACTEClassOwner.Logo <> '' then
    begin
      FieldByName('Imagem').AsString := DACTEClassOwner.Logo;
      vStream := TMemoryStream.Create;
      try
        if FileExists(DACTEClassOwner.Logo) then
           vStream.LoadFromFile(DACTEClassOwner.Logo)
        else
        begin
           vStringStream:= TStringStream.Create(DACTEClassOwner.Logo);
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

    if FDACTEClassOwner.Sistema <> '' then
      FieldByName('Sistema').AsString := FDACTEClassOwner.Sistema
    else
      FieldByName('Sistema').AsString := 'Projeto ACBr - http://acbr.sf.net';

    if FDACTEClassOwner.Usuario <> '' then
      FieldByName('Usuario').AsString := ' - ' + FDACTEClassOwner.Usuario
    else
      FieldByName('Usuario').AsString := '';

    if FDACTEClassOwner.Fax <> '' then
      FieldByName('Fax').AsString := ' - FAX ' + FDACTEClassOwner.Fax
    else
      FieldByName('Fax').AsString := '';

    FieldByName('Site').AsString := FDACTEClassOwner.Site;
    FieldByName('Email').AsString := FDACTEClassOwner.Email;

    if FDACTEClassOwner.ImprimirDescPorc then
      FieldByName('Desconto').AsString := 'DESC %'
    else
      FieldByName('Desconto').AsString := 'V.DESC.';

    if ((FCTe.Ide.tpEmis = teNormal) or (FCTe.Ide.tpEmis = teSCAN)) then
    begin
      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
      FieldByName('Contingencia_ID').AsString := '';

      if ((FDACTEClassOwner.CTeCancelada) or (FCTe.procCTe.cStat = 101)) then
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO'
      else
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

      if EstaVazio(FDACTEClassOwner.ProtocoloCTE) then
      begin
        if not (FCTe.Ide.tpEmis in [teContingencia, teFSDA]) and EstaVazio(FCTe.procCTe.nProt) then
          FieldByName('Contingencia_Valor').AsString := 'CTe sem Autorização de Uso da SEFAZ'
        else
          FieldByName('Contingencia_Valor').AsString := FCTe.procCTe.nProt + ' ' + SeSenao(FCTe.procCTe.dhRecbto <> 0,
            DateTimeToStr(FCTe.procCTe.dhRecbto), '');
      end
      else
        FieldByName('Contingencia_Valor').AsString := FDACTEClassOwner.ProtocoloCTE;
    end
    else
    begin
      vChave_Contingencia := CTeUtil.GerarChaveContingencia(FCTe);
      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
      FieldByName('Contingencia_ID').AsString := vChave_Contingencia;

      if ((FCTe.Ide.tpEmis = teContingencia) or (FCTe.Ide.tpEmis = teFSDA)) then
      begin
        FieldByName('Contingencia_Descricao').AsString := 'DADOS DA CT-E';
        FieldByName('Contingencia_Valor').AsString := CTeUtil.FormatarChaveContingencia(vChave_Contingencia);
      end
      else
        if (FCTe.Ide.tpEmis = teDPEC) then
        begin
          FieldByName('Contingencia_Descricao').AsString := 'NÚMERO DE REGISTRO DPEC';

          //precisa testar
  //        if EstaVazio(FDACTEClassOwner.ProtocoloCTE) then
  //          raise EACBrCTeException.Create('Protocolo de Registro no DPEC não informado.')
  //        else
  //          FieldByName('Contingencia_Valor').AsString := FDACTEClassOwner.ProtocoloCTe;
        end
        else // 25/06/13 - Wislei - Adicionado a verificação para os SVC.
          if (FCTe.Ide.tpEmis = teSVCSP) or (FCTe.Ide.tpEmis = teSVCRS) then
          begin
            FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';
            FieldByName('Contingencia_Valor').AsString := FCTe.procCTe.nProt + ' ' + SeSenao(FCTe.procCTe.dhRecbto <> 0,
              DateTimeToStr(FCTe.procCTe.dhRecbto), '');
          end;
    end;

    //    FieldByName('LinhasPorPagina').AsInteger := FDACTEClassOwner.ProdutosPorPagina;
    Post;
  end;
end;

procedure TdmACBrCTeFR.CarregaRecebedor;
begin
  { Recebedor }
  with cdsRecebedor do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('CNPJ', ftString, 18);
    FieldDefs.Add('XNome', ftString, 60);
    FieldDefs.Add('XFant', ftString, 60);
    FieldDefs.Add('XLgr', ftString, 60);
    FieldDefs.Add('Nro', ftString, 60);
    FieldDefs.Add('XCpl', ftString, 60);
    FieldDefs.Add('XBairro', ftString, 60);
    FieldDefs.Add('CMun', ftString, 7);
    FieldDefs.Add('XMun', ftString, 60);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('CEP', ftString, 9);
    FieldDefs.Add('CPais', ftString, 4);
    FieldDefs.Add('XPais', ftString, 60);
    FieldDefs.Add('Fone', ftString, 15);
    FieldDefs.Add('IE', ftString, 20);
    FieldDefs.Add('IM', ftString, 15);
    FieldDefs.Add('IEST', ftString, 20);
    FieldDefs.Add('CRT', ftString, 1);

    CreateDataSet;
    Append;

    with FCTE.Receb do
    begin
      FieldByName('CNPJ').AsString := FormatarCNPJCPF(CNPJCPF);
      FieldByName('XNome').AsString := XNome;
      with EnderReceb do
      begin
        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('CPais').AsString := IntToStr(CPais);
        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
    end;

    Post;
  end;

end;

procedure TdmACBrCTeFR.CarregaRemetente;
begin
  { Remetente }
  with cdsRemetente do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('CNPJ', ftString, 18);
    FieldDefs.Add('XNome', ftString, 60);
    FieldDefs.Add('XFant', ftString, 60);
    FieldDefs.Add('XLgr', ftString, 60);
    FieldDefs.Add('Nro', ftString, 60);
    FieldDefs.Add('XCpl', ftString, 60);
    FieldDefs.Add('XBairro', ftString, 60);
    FieldDefs.Add('CMun', ftString, 7);
    FieldDefs.Add('XMun', ftString, 60);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('CEP', ftString, 9);
    FieldDefs.Add('CPais', ftString, 4);
    FieldDefs.Add('XPais', ftString, 60);
    FieldDefs.Add('Fone', ftString, 15);
    FieldDefs.Add('IE', ftString, 20);
    FieldDefs.Add('IM', ftString, 15);
    FieldDefs.Add('IEST', ftString, 20);
    FieldDefs.Add('CRT', ftString, 1);

    CreateDataSet;
    Append;
    with FCTE.Rem do
    begin
      FieldByName('CNPJ').AsString := FormatarCNPJCPF(CNPJCPF);
      FieldByName('XNome').AsString := XNome;
      FieldByName('XFant').AsString := XFant;
      with EnderReme do
      begin
        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('CPais').AsString := IntToStr(CPais);
        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
    end;
    Post;
  end;
end;

procedure TdmACBrCTeFR.CarregaSeguro;
var
  i: Integer;
begin
  with cdsSeguro do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('RESPONSAVEL', ftString, 60);
    FieldDefs.Add('NOMESEGURADORA', ftString, 60);
    FieldDefs.Add('NUMEROAPOLICE', ftString, 60);
    FieldDefs.Add('NUMEROAVERBACAO', ftString, 60);
    CreateDataSet;
//    Append;
    //
    {$IFDEF PL_200}
    if CTe.infCTeNorm.seg.Count > 0 then
    begin
      for I := 0 to CTe.infCTeNorm.seg.Count - 1 do
      begin
        with CTe.infCTeNorm.seg.Items[I] do
        begin
    {$ELSE}
    if CTe.InfSeg.Count > 0 then
    begin
      for I := 0 to CTe.InfSeg.Count - 1 do
      begin
        with CTe.InfSeg.Items[I] do
        begin
    {$ENDIF}
          Append;
          case respSeg of
            rsRemetente: FieldByName('RESPONSAVEL').AsString := 'Remetente';
            rsExpedidor: FieldByName('RESPONSAVEL').AsString := 'Expedidor';
            rsRecebedor: FieldByName('RESPONSAVEL').AsString := 'Recebedor';
            rsDestinatario: FieldByName('RESPONSAVEL').AsString := 'Destinatário';
            rsEmitenteCTe: FieldByName('RESPONSAVEL').AsString := 'Emitente';
            rsTomadorServico: FieldByName('RESPONSAVEL').AsString := 'Tomador';
          end;
          FieldByName('NOMESEGURADORA').AsString := xSeg;
          FieldByName('NUMEROAPOLICE').AsString := nApol;
          FieldByName('NUMEROAVERBACAO').AsString := nAver;
          Post;
        end;
      end;
    end
    else
    begin
      Append;
      FieldByName('RESPONSAVEL').AsString := '';
      FieldByName('NOMESEGURADORA').AsString := '';
      FieldByName('NUMEROAPOLICE').AsString := '';
      FieldByName('NUMEROAVERBACAO').AsString := '';
      Post;
    end;

  end;

end;

procedure TdmACBrCTeFR.CarregaTomador;
begin
  { Tomador Outros }
  with cdsTomador do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('CNPJ', ftString, 18);
    FieldDefs.Add('IE', ftString, 20);
    FieldDefs.Add('XNome', ftString, 60);
    FieldDefs.Add('XFant', ftString, 60);
    FieldDefs.Add('Fone', ftString, 15);
    FieldDefs.Add('XLgr', ftString, 255);
    FieldDefs.Add('Nro', ftString, 60);
    FieldDefs.Add('XCpl', ftString, 60);
    FieldDefs.Add('XBairro', ftString, 60);
    FieldDefs.Add('CMun', ftString, 7);
    FieldDefs.Add('XMun', ftString, 60);
    FieldDefs.Add('UF', ftString, 2);
    FieldDefs.Add('CEP', ftString, 9);
    FieldDefs.Add('CPais', ftString, 4);
    FieldDefs.Add('XPais', ftString, 60);
    CreateDataSet;
    Append;

    case FCTe.Ide.Toma03.Toma of
      tmRemetente:
        begin
          FieldByName('CNPJ').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
          FieldByName('XNome').AsString := FCTe.Rem.xNome;
          FieldByName('XFant').AsString := FCTe.Rem.xFant;
          FieldByName('IE').AsString := FCTe.Rem.IE;
          FieldByName('Xlgr').AsString := FCTe.Rem.EnderReme.xLgr;
          FieldByName('Nro').AsString := FCTe.Rem.EnderReme.nro;
          FieldByName('XCpl').AsString := FCTe.Rem.EnderReme.xCpl;
          FieldByName('XBairro').AsString := FCTe.Rem.EnderReme.xBairro;
          FieldByName('CMun').AsString := IntToStr(FCTe.Rem.EnderReme.cMun);
          FieldByName('XMun').AsString := FCTe.Rem.EnderReme.xMun;
          FieldByName('UF').AsString := FCTe.Rem.EnderReme.UF;
          FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(FCTe.Rem.EnderReme.CEP,8));
          FieldByName('CPais').AsString := IntToStr(FCTe.Rem.EnderReme.cPais);
          FieldByName('XPais').AsString := FCTe.Rem.EnderReme.xPais;
          { Alterado por Jose Nilton Pace em 16/05/2013 }
          FieldByName('Fone').AsString := FormatarFone(FCTe.Rem.fone);
        end;

      tmDestinatario:
        begin
          FieldByName('CNPJ').AsString := FormatarCNPJCPF(FCTe.Dest.CNPJCPF);
          FieldByName('XNome').AsString := FCTe.Dest.xNome;
          FieldByName('IE').AsString := FCTe.Dest.IE;
          FieldByName('Xlgr').AsString := FCTe.Dest.EnderDest.xLgr;
          FieldByName('Nro').AsString := FCTe.Dest.EnderDest.nro;
          FieldByName('XCpl').AsString := FCTe.Dest.EnderDest.xCpl;
          FieldByName('XBairro').AsString := FCTe.Dest.EnderDest.xBairro;
          FieldByName('CMun').AsString := IntToStr(FCTe.Dest.EnderDest.cMun);
          FieldByName('XMun').AsString := FCTe.Dest.EnderDest.xMun;
          FieldByName('UF').AsString := FCTe.Dest.EnderDest.UF;
          FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(FCTe.Dest.EnderDest.CEP,8));
          FieldByName('CPais').AsString := IntToStr(FCTe.Dest.EnderDest.cPais);
          FieldByName('XPais').AsString := FCTe.Dest.EnderDest.xPais;
          { Alterado por Jose Nilton Pace em 16/05/2013 }
          FieldByName('Fone').AsString := FormatarFone(FCTe.Dest.fone);
        end;

      tmExpedidor:
        begin
          FieldByName('CNPJ').AsString := FormatarCNPJCPF(FCTe.Exped.CNPJCPF);
          FieldByName('XNome').AsString := FCTe.Exped.xNome;
          FieldByName('IE').AsString := FCTe.Exped.IE;
          FieldByName('Xlgr').AsString := FCTe.Exped.EnderExped.xLgr;
          FieldByName('Nro').AsString := FCTe.Exped.EnderExped.nro;
          FieldByName('XCpl').AsString := FCTe.Exped.EnderExped.xCpl;
          FieldByName('XBairro').AsString := FCTe.Exped.EnderExped.xBairro;
          FieldByName('CMun').AsString := IntToStr(FCTe.Exped.EnderExped.cMun);
          FieldByName('XMun').AsString := FCTe.Exped.EnderExped.xMun;
          FieldByName('UF').AsString := FCTe.Exped.EnderExped.UF;
          FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(FCTe.Exped.EnderExped.CEP,8));
          FieldByName('CPais').AsString := IntToStr(FCTe.Exped.EnderExped.cPais);
          FieldByName('XPais').AsString := FCTe.Exped.EnderExped.xPais;
          { Alterado por Jose Nilton Pace em 16/05/2013 }
          FieldByName('Fone').AsString := FormatarFone(FCTe.Exped.fone);
        end;

      tmRecebedor:
        begin
          FieldByName('CNPJ').AsString := FormatarCNPJCPF(FCTe.Receb.CNPJCPF);
          FieldByName('XNome').AsString := FCTe.Receb.xNome;
          FieldByName('IE').AsString := FCTe.Receb.IE;
          FieldByName('Xlgr').AsString := FCTe.Receb.EnderReceb.xLgr;
          FieldByName('Nro').AsString := FCTe.Receb.EnderReceb.nro;
          FieldByName('XCpl').AsString := FCTe.Receb.EnderReceb.xCpl;
          FieldByName('XBairro').AsString := FCTe.Receb.EnderReceb.xBairro;
          FieldByName('CMun').AsString := IntToStr(FCTe.Receb.EnderReceb.cMun);
          FieldByName('XMun').AsString := FCTe.Receb.EnderReceb.xMun;
          FieldByName('UF').AsString := FCTe.Receb.EnderReceb.UF;
          FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(FCTe.Receb.EnderReceb.CEP,8));
          FieldByName('CPais').AsString := IntToStr(FCTe.Receb.EnderReceb.cPais);
          FieldByName('XPais').AsString := FCTe.Receb.EnderReceb.xPais;
          { Alterado por Jose Nilton Pace em 16/05/2013 }
          FieldByName('Fone').AsString := FormatarFone(FCTe.Receb.fone);
        end;
    end;

    case FCTe.Ide.Toma4.Toma of
      tmOutros:
        begin
          FieldByName('CNPJ').AsString := FormatarCNPJCPF(FCTe.Ide.Toma4.CNPJCPF);
          FieldByName('XNome').AsString := FCTe.Ide.Toma4.xNome;
          FieldByName('IE').AsString := FCTe.Ide.Toma4.IE;
          FieldByName('Xlgr').AsString := FCTe.Ide.Toma4.EnderToma.xLgr;
          FieldByName('Nro').AsString := FCTe.Ide.Toma4.EnderToma.nro;
          FieldByName('XCpl').AsString := FCTe.Ide.Toma4.EnderToma.xCpl;
          FieldByName('XBairro').AsString := FCTe.Ide.Toma4.EnderToma.xBairro;
          FieldByName('CMun').AsString := IntToStr(FCTe.Ide.Toma4.EnderToma.cMun);
          FieldByName('XMun').AsString := FCTe.Ide.Toma4.EnderToma.xMun;
          FieldByName('UF').AsString := FCTe.Ide.Toma4.EnderToma.UF;
          FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(FCTe.Ide.Toma4.EnderToma.CEP,8));
          FieldByName('CPais').AsString := IntToStr(FCTe.Ide.Toma4.EnderToma.cPais);
          FieldByName('XPais').AsString := FCTe.Ide.Toma4.EnderToma.xPais;
          { Alterado por Jose Nilton Pace em 16/05/2013 }
          FieldByName('Fone').AsString := FormatarFone(FCTe.Ide.Toma4.fone);
        end;
    end;
    Post;
  end;

end;

procedure TdmACBrCTeFR.CarregaVolumes;
var
  I, J: Integer;
  MCub, Volumes, VlrServico: Currency;
  ProdutoPred, OutrasCaract: string;
  TipoMedida: array of string;
  UnidMedida: array of string;
  QdtMedida: array of currency;
begin
  with cdsVolumes do
  begin
    Close;
    FieldDefs.Clear;

    FieldDefs.Add('Produto', ftString, 100);
    FieldDefs.Add('CaracteristicaCarga', ftString, 100);
    FieldDefs.Add('ValorServico', ftFloat);

    FieldDefs.Add('DescTipo', ftString, 60);
    FieldDefs.Add('UnMedida', ftString, 6);
    FieldDefs.Add('QMedida', ftFloat);
    FieldDefs.Add('MCub', ftFloat);
    FieldDefs.Add('QVol', ftFloat);

    CreateDataSet;
    j := 0;
    VlrServico := 0;
    MCub := 0;
    Volumes := 0;

    {$IFDEF PL_200}
    for I := 0 to CTe.infCTeNorm.infCarga.infQ.Count - 1 do
    begin
      with CTe.infCTeNorm.infCarga do
      begin
    {$ELSE}
    for I := 0 to CTe.InfCarga.InfQ.Count - 1 do
    begin
      with CTe.InfCarga do
      begin
    {$ENDIF}
        ProdutoPred := proPred;
        OutrasCaract := xOutCat;

        {$IFDEF PL_103}
        VlrServico := CTe.InfCarga.vMerc;
        {$ELSE}
        VlrServico := vCarga;
        {$ENDIF}

        case InfQ.Items[I].cUnid of
          uM3: MCub := MCub+InfQ.Items[I].qCarga;
          uUNIDADE: Volumes := Volumes+InfQ.Items[I].qCarga;
        end;
//          else

// Alterado por NCC em 23/10/2014
// Alterei para casos onde há volumes distintos, mesmo sendo em unidades.
// Exemplo, dois fardos com quantidades diferentes cada fardo.
// Ver http://www.projetoacbr.com.br/forum/index.php?/topic/2049-colocar-volume-no-cte-%C3%A9-possivel/
        begin
          Inc(J);
          SetLength(TipoMedida, j);
          SetLength(UnidMedida, j);
          SetLength(QdtMedida, j);
          TipoMedida[J - 1] := InfQ.Items[I].tpMed;
          QdtMedida[J - 1] := InfQ.Items[I].qCarga;

          case InfQ.Items[I].cUnid of
            uKG: UnidMedida[J - 1] := 'KG';
            uTON: UnidMedida[J - 1] := 'TON';
            uLITROS: UnidMedida[J - 1] := 'LT';
            uMMBTU: UnidMedida[J - 1] := 'MMBTU';
            uUNIDADE: UnidMedida[J - 1] := 'UND';
            uM3: UnidMedida[J - 1] := 'M3';
          end;
        end;
        //end;
      end;
    end;

    { Alterado por Jose Nilton Pace em 16/05/2013 }
    if j = 0 then begin
      Append;
      FieldByName('Produto').AsString := ProdutoPred;
      FieldByName('CaracteristicaCarga').AsString := OutrasCaract;
      FieldByName('ValorServico').AsFloat := VlrServico;
      FieldByName('MCub').AsFloat := MCub;
      FieldByName('QVol').AsFloat := Volumes;
      Post;
    end else
       for I := 0 to j - 1 do
       begin
         Append;
         FieldByName('Produto').AsString := ProdutoPred;
         FieldByName('CaracteristicaCarga').AsString := OutrasCaract;
         FieldByName('ValorServico').AsFloat := VlrServico;
         FieldByName('MCub').AsFloat := MCub;
         FieldByName('QVol').AsFloat := Volumes;
         FieldByName('UnMedida').AsString := UnidMedida[i];
         FieldByName('DescTipo').AsString := TipoMedida[i];
         FieldByName('QMedida').AsFloat := QdtMedida[i];
         Post;
       end;
  end;
end;

constructor TdmACBrCTeFR.Create(AOwner: TComponent);
begin
  inherited;
  FDACTEClassOwner := TACBrCteDACTEClass(AOwner);
end;

procedure TdmACBrCTeFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  Child: TfrxChild;
  DetailData: TfrxDetailData;
  Memo: TfrxMemoView;
  Shape: TfrxShapeView;
begin
  case TipoEvento of
    teCCe:
    begin
      //Esconde ChildJustificativa
      Memo:= frxReport.FindObject('JustTit') as TfrxMemoView;
      if Memo <> nil then
      begin
        Memo.Visible:= False;
      end;
      Memo:= frxReport.FindObject('JustDesc') as TfrxMemoView;
      if Memo <> nil then
      begin
        Memo.Visible:= False;
      end;
      Shape:= frxReport.FindObject('ShapeJust') as TfrxShapeView;
      if Shape <> nil then
      begin
        Shape.Visible:= False;
      end;
      Child:= frxReport.FindObject('ChildJustificativa') as TfrxChild;
      if Child <> nil then
      begin
        Child.Height:= 0;
      end;
    end;
    teCancelamento:
    begin
      //Esconde ChildCondUso
      Child:= frxReport.FindObject('ChildCondUso') as TfrxChild;
      if Child <> nil then
      begin
        Child.Visible:= False;
      end;

      //Esconde ChildCorrecao
      Child:= frxReport.FindObject('ChildCorrecao') as TfrxChild;
      if Child <> nil then
      begin
        Child.Visible:= False;
      end;

      //Esconde DetailData1
      DetailData:= frxReport.FindObject('DetailData1') as TfrxDetailData;
      if DetailData <> nil then
      begin
        DetailData.Visible:= False;
      end;
    end;
  end;
end;

end.

