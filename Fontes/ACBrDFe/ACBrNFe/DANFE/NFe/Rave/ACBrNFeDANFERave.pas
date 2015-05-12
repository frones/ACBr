{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                          }
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
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 09/03/2009: Dulcemar P.Zilli
|*  - Correcao impressão informações IPI
|* 11/03/2008: Dulcemar P.Zilli
|*  - Inclusão Observações Fisco
|* 11/03/2008: Dulcemar P.Zilli
|*  - Inclusão totais ISSQN
|* 23/06/2009: João H. Souza
|*  - Alterações diversas
******************************************************************************}
{$I ACBr.inc}

unit ACBrNFeDANFERave;

interface

uses Forms, SysUtils, Classes, Graphics,
  RpBase, RpCon, RpConDS, RpDefine, RpDevice, RpRave, RpSystem, RvClass,
  RvCsData, RvCsDraw, RvCsStd, RvCsRpt, RvData, RvDefine, RvUtil, RVProj,
  RvDirectDataView, RVCsBars, RVDataField,
  ACBrNFeDANFEClass, ACBrNFeDANFERaveDM, pcnNFe, pcnConversao;

type
  TACBrNFeDANFERave = class( TACBrNFeDANFEClass )
   private
    FdmDanfe : TdmACBrNFeRave;
    FRaveFile: String;
    FEspessuraBorda: Integer;
    FTamanhoFonte_RazaoSocial: Integer;
    FTamanhoFonte_ANTT: Integer;
    FTributosFonte: string;
    FTributosPercentual: TpcnPercentualTributos;
    FMarcaDaguaMSG: string;
    procedure ExecutaReport;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnPrintRaveSystem(Sender: TObject);
    procedure ImprimirDANFE(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEPDF(NFE : TNFe = nil); override ;
  published
    property RavFile : String read FRaveFile write FRaveFile;
    property dmDanfe : TdmACBrNFeRave read FdmDanfe write FdmDanfe;
    property EspessuraBorda : Integer read FEspessuraBorda write FEspessuraBorda;
    property TamanhoFonte_RazaoSocial: Integer read FTamanhoFonte_RazaoSocial write FTamanhoFonte_RazaoSocial;
    property TamanhoFonte_ANTT: Integer read FTamanhoFonte_ANTT write FTamanhoFonte_ANTT;
    property TributosFonte: string read FTributosFonte write FTributosFonte;
    property TributosPercentual: TpcnPercentualTributos read FTributosPercentual write FTributosPercentual;
    property MarcaDaguaMSG: string read FMarcaDaguaMSG write FMarcaDaguaMSG;
  end;

implementation

uses ACBrNFe, ACBrUtil, ACBrDFeUtil, StrUtils, Dialogs;

constructor TACBrNFeDANFERave.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FdmDanfe := TdmACBrNFeRave.Create(Self);
  FRaveFile := '' ;
  FEspessuraBorda := 1;
  FTamanhoFonte_RazaoSocial := 12;
  FTamanhoFonte_ANTT := 10;
  FTributosPercentual := ptValorProdutos;
  FMarcaDaguaMSG:='';
end;

destructor TACBrNFeDANFERave.Destroy;
begin

  dmDanfe.Free;
  inherited Destroy ;
end;

procedure TACBrNFeDANFERave.ExecutaReport;
   function wDisplayFormat(Casas: integer):string;
   var
      i: integer;
      wzeros: string;
   begin
      if Casas=0 then
         Result:='#0'
      else
      begin
         Result:='#,';
         wzeros:='0.';
         for I := 1 to Casas do
         begin
            Result:=Result+'#';
            wZeros:=wZeros+'0';
         end;
         Result:=Result+wZeros;
      end;
   end;
var
   i,j: integer;
   k: double;

   wReport: TRaveReport;
   wRegion: array [1..2] of TRaveRegion;
   wSection: TRaveSection;
   wBand: TRaveBand;
   wDataView: TRaveDataView;
   wPage: array[1..2] of TRavePage;
   wBarcode: TRaveCode128BarCode;
   wBitmap: array[1..1] of TRaveBitmap;
   wHLine: array[1..32] of TRaveHLine;
   wVLine: array[1..32] of TRaveVLine;
   wRectangle: array[1..7] of TRaveRectangle;
   wSquare: array[1..1] of TRaveSquare;
   wText: array[1..17] of TRaveText;
   wDataText: array[1..21] of TRaveDataText;
   wFloatField: array[1..2] of TRaveFloatField;
   wDataMemo: array[1..1] of TRaveDataMemo;

   vMargemInferiorAtual, vMargemInferior, vHeightPadrao: double;
   vMargemSuperiorAtual, vMargemSuperior: double;
begin
   try
      dmDanfe.RvProject.Open;
      with dmDanfe.RvProject.ProjMan do
      begin
         //descricao CST ou CSON
         if (dmDanfe.NFe.Emit.CRT = crtSimplesNacional) then
         begin
            wPage[1] := FindRaveComponent('GlobalDadosProdutos',nil) as TRavePage;
            wText[1] := FindRaveComponent('Text1',wPage[1]) as TRaveText;
            if (wText[1] <> nil) then
            begin
               wText[1].Font.Height:=5;
               wText[1].Top:=wText[1].Top+0.02;
               wText[1].Text:='CSOSN';
            end;
         end;

         //Tamanho Fonte Razao Social
         if (FTamanhoFonte_RazaoSocial <> 12) then //12=tamanho padrao que esta nos .RAV
         begin
            wPage[1] := FindRaveComponent('GlobalDANFE',nil) as TRavePage;
            wDataText[4] := FindRaveComponent('DataText'+inttostr(4),wPage[1]) as TRaveDataText;
            if (wDataText[4] <> nil) then
               wDataText[4].Font.Height:=FTamanhoFonte_RazaoSocial;
         end;

         //Tamanho Fonte ANTT
         if (FTamanhoFonte_ANTT <> 8) then //8=tamanho padrao que esta nos .RAV
         begin
            wPage[1] := FindRaveComponent('GlobalTransportador',nil) as TRavePage;
            wDataText[16] := FindRaveComponent('DataText'+inttostr(16),wPage[1]) as TRaveDataText;
            if (wDataText[16] <> nil) then
               wDataText[16].Font.Height:=FTamanhoFonte_ANTT;
         end;

         //Tamanho Fonte Demais Campos
         if (FTamanhoFonte_DemaisCampos <> 8) then //8=tamanho padrao que esta nos .RAV
         begin
            wPage[1] := FindRaveComponent('GlobalDANFE',nil) as TRavePage;
            i:=22;
            while i>0 do
            begin
               if (i in [9,10,11,12,17]) then
               begin
                  wDataText[i] := FindRaveComponent('DataText'+inttostr(i),wPage[1]) as TRaveDataText;
                  if (wDataText[i] <> nil) then
                     wDataText[i].Font.Height:=FTamanhoFonte_DemaisCampos;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalDestinatario',nil) as TRavePage;
            i:=22;
            while i>0 do
            begin
               if (i in [14,15,16,1,2,3,4,5,6,7,9,17,10,13,18,20,21,11,12,19]) then
               begin
                  wDataText[i] := FindRaveComponent('DataText'+inttostr(i),wPage[1]) as TRaveDataText;
                  if (wDataText[i] <> nil) then
                     wDataText[i].Font.Height:=FTamanhoFonte_DemaisCampos;
               end;
               i:=i-1;
            end;
            // mudança de cor para mensagem de NFe Cancelada (somente em producao)
            wDataText[8] := FindRaveComponent('DataText8',wPage[1]) as TRaveDataText;
            if (wDataText[8] <> nil) then
            begin
               if (wDataText[8].DataField = 'Mensagem0') and
                  (dmDanfe.NFe.procNFe.cStat = 101) and
                  (dmDanfe.NFe.procNFe.tpAmb = taProducao) then
                  wDataText[8].Font.Color := clRed
               else
                  wDataText[8].Font.Color := clSilver;
            end;

            wPage[1] := FindRaveComponent('GlobalFatura',nil) as TRavePage;
            wDataText[15] := FindRaveComponent('Fatura_nFat',wPage[1]) as TRaveDataText;
            wDataText[16] := FindRaveComponent('Fatura_vOrig',wPage[1]) as TRaveDataText;
            wDataText[17] := FindRaveComponent('Fatura_vDesc',wPage[1]) as TRaveDataText;
            wDataText[18] := FindRaveComponent('Fatura_vLiq',wPage[1]) as TRaveDataText;
            i:=22;
            while i>0 do
            begin
               if (i in [5,11,1,7,15,16,17,18]) then
               begin
                  if (i in [5,11,1,7]) then
                     wDataText[i] := FindRaveComponent('DataText'+inttostr(i),wPage[1]) as TRaveDataText;
                  if (wDataText[i] <> nil) then
                     wDataText[i].Font.Height:=FTamanhoFonte_DemaisCampos;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalCalculoImposto',nil) as TRavePage;
            i:=22;
            while i>0 do
            begin
               if (i in [14,8,9,15,16,5,4,6,3,2,1]) then
               begin
                  wDataText[i] := FindRaveComponent('DataText'+inttostr(i),wPage[1]) as TRaveDataText;
                  if (wDataText[i] <> nil) then
                     wDataText[i].Font.Height:=FTamanhoFonte_DemaisCampos;
               end;
               i:=i-1;
            end;
            //vTotTrib
            if dmDanfe.NFe.Total.ICMSTot.vTotTrib <> 0 then
            begin
              wText[7] := FindRaveComponent('Text7',wPage[1]) as TRaveText;
              wDataText[7] := FindRaveComponent('DataText7',wPage[1]) as TRaveDataText;
              if ((wDataText[7] <> nil) and (wText[7] <> nil)) then
              begin
                wText[7].Text := 'V.APROX.TRIBUTOS';
                if NaoEstaVazio(FTributosFonte) then
                  wText[7].Text := wText[7].Text+' (Fonte:'+FTributosFonte+')';
              end;
            end
            else
            begin
              wText[7] := FindRaveComponent('Text7',wPage[1]) as TRaveText;
              wDataText[7] := FindRaveComponent('DataText7',wPage[1]) as TRaveDataText;
              wVLine[10] := FindRaveComponent('VLine10',wPage[1]) as TRaveVLine;
              if ((wDataText[7] <> nil) and (wText[7] <> nil) and (wVLine[10] <> nil)) then
              begin
                wText[7].Text := 'V.Aprox.Tributos';
                if NaoEstaVazio(FTributosFonte) then
                  wText[7].Text := wText[7].Text+'-Fonte:'+FTributosFonte;
                wVLine[6] := FindRaveComponent('VLine6',wPage[1]) as TRaveVLine;
                k := wVLine[6].Left-wVLine[10].Left;

                //move quadros
                wText[17] := FindRaveComponent('Text17',wPage[1]) as TRaveText;
                wText[17].Left := wText[17].Left + k;
                wDataText[15] := FindRaveComponent('DataText15',wPage[1]) as TRaveDataText;
                wDataText[15].Left := wDataText[15].Left + k;
                wVLine[7] := FindRaveComponent('VLine7',wPage[1]) as TRaveVLine;
                wVLine[7].Left := wVLine[7].Left + k;

                wText[11] := FindRaveComponent('Text11',wPage[1]) as TRaveText;
                wText[11].Left := wText[11].Left + k;
                wDataText[9] := FindRaveComponent('DataText9',wPage[1]) as TRaveDataText;
                wDataText[9].Left := wDataText[9].Left + k;
                wVLine[4] := FindRaveComponent('VLine4',wPage[1]) as TRaveVLine;
                wVLine[4].Left := wVLine[4].Left + k;

                wText[10] := FindRaveComponent('Text10',wPage[1]) as TRaveText;
                wText[10].Left := wText[10].Left + (k/2);
                wDataText[8] := FindRaveComponent('DataText8',wPage[1]) as TRaveDataText;
                wDataText[8].Left := wDataText[8].Left + (k/2);
                wDataText[8].width := wDataText[8].width + (k/2);
                wVLine[3] := FindRaveComponent('VLine3',wPage[1]) as TRaveVLine;
                wVLine[3].Left := wVLine[3].Left + (k/2);

                wDataText[14] := FindRaveComponent('DataText14',wPage[1]) as TRaveDataText;
                wDataText[14].width := wDataText[14].width + (k/2);

                wText[7].Left := 30;
                wDataText[7].Left := 30;
                wVLine[10].Left := 30;
              end;
            end;

            wPage[1] := FindRaveComponent('GlobalTransportador',nil) as TRavePage;
            i:=22;
            while i>0 do
            begin
               if (i in [14,8,9,10,1,2,3,4,5,11,6,7,17,12]) then
               begin
                  wDataText[i] := FindRaveComponent('DataText'+inttostr(i),wPage[1]) as TRaveDataText;
                  if (wDataText[i] <> nil) then
                     wDataText[i].Font.Height:=FTamanhoFonte_DemaisCampos;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalCalculoISSQN',nil) as TRavePage;
            i:=22;
            while i>0 do
            begin
               if (i in [14,8,15,16]) then
               begin
                  wDataText[i] := FindRaveComponent('DataText'+inttostr(i),wPage[1]) as TRaveDataText;
                  if (wDataText[i] <> nil) then
                     wDataText[i].Font.Height:=FTamanhoFonte_DemaisCampos;
               end;
               i:=i-1;
            end;
         end;

         //Expande LOGO
         if FExpandirLogoMarca then
         begin
            wPage[1] := FindRaveComponent('GlobalDANFE',nil) as TRavePage;
            wBitmap[1] := FindRaveComponent('Bitmap1',wPage[1]) as TRaveBitmap;
            if (wBitmap[1] <> nil) then
            begin
               wBitmap[1].BringToFront;
               wBitmap[1].Top:=0.060;
               wBitmap[1].Width:=3.190;
               wBitmap[1].Height:=1.110;
            end;

            i:=20;
            while i>0 do
            begin
               if (i in [2,4,8,15,16,18,20]) then
               begin
                  wDataText[i] := FindRaveComponent('DataText'+inttostr(i),wPage[1]) as TRaveDataText;
                  if (wDataText[i] <> nil) then
                     wDataText[i].Left:=30;
               end;
               i:=i-1;
            end;

            wDataMemo[1] := FindRaveComponent('DataMemo1',wPage[1]) as TRaveDataMemo;
            if (wDataMemo[1] <> nil) then
               wDataMemo[1].Left:=30;

         end;

         //Formulario Continuo
         if FFormularioContinuo then
         begin
            //canhoto
            wPage[1] := FindRaveComponent('GlobalRecibo',nil) as TRavePage;
            wDataText[1] := FindRaveComponent('DataText1',wPage[1]) as TRaveDataText;
            if (wDataText[1] <> nil) then
               wDataText[1].Left:=30;
            i:=3;
            while i>0 do
            begin
               wText[i] := FindRaveComponent('Text'+inttostr(i),wPage[1]) as TRaveText;
               if (wText[i] <> nil) then
                  wText[i].Left:=30;
               i:=i-1;
            end;
            wRectangle[1] := FindRaveComponent('Rectangle1',wPage[1]) as TRaveRectangle;
            if (wRectangle[1] <> nil) then
               wRectangle[1].Left:=30;
            i:=2;
            while i>0 do
            begin
               wHLine[i] := FindRaveComponent('HLine'+inttostr(i),wPage[1]) as TRaveHLine;
               if (wHLine[i] <> nil) then
                  wHline[i].Left:=30;
               i:=i-1;
            end;
            i:=2;
            while i>0 do
            begin
               wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
               if (wVLine[i] <> nil) then
                  wVline[i].Left:=30;
               i:=i-1;
            end;
            //cabecalho e dados do emitente
            wPage[1] := FindRaveComponent('GlobalDANFE',nil) as TRavePage;
            i:=7;
            while i>0 do
            begin
               wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
               if (wRectangle[i] <> nil) then
                  wRectangle[i].Left:=30;
               i:=i-1;
            end;
            wSquare[1] := FindRaveComponent('Square1',wPage[1]) as TRaveSquare;
            if (wSquare[1] <> nil) then
               wSquare[1].Left:=30;
            wBitmap[1] := FindRaveComponent('Bitmap1',wPage[1]) as TRaveBitmap;
            if (wBitmap[1] <> nil) then
               wBitmap[1].Left:=30;
            i:=4;
            while i>0 do
            begin
               wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
               if (wVLine[i] <> nil) then
                  wVline[i].Left:=30;
               i:=i-1;
            end;
            i:=13;
            while i>0 do
            begin
               wText[i] := FindRaveComponent('Text'+inttostr(i),wPage[1]) as TRaveText;
               if (wText[i] <> nil) then
                  wText[i].Left:=30;
               i:=i-1;
            end;
            wDataMemo[1] := FindRaveComponent('DataMemo1',wPage[1]) as TRaveDataMemo;
            if (wDataMemo[1] <> nil) then
               wDataMemo[1].Left:=30;
            i:=20;
            while i>0 do
            begin
               if (i in [2,3,4,8,10,12,14,15,16,18,20]) then
               begin
                  wDataText[i] := FindRaveComponent('DataText'+inttostr(i),wPage[1]) as TRaveDataText;
                  if (wDataText[i] <> nil) then
                     wDataText[i].Left:=30;
               end;
               i:=i-1;
            end;
         end;

         //contingencia
         wPage[1] := FindRaveComponent('GlobalDANFE',nil) as TRavePage;
         if ((dmDanfe.NFe.Ide.tpEmis = teNormal) or
             (dmDanfe.NFe.Ide.tpEmis = teSVCAN) or
             (dmDanfe.NFe.Ide.tpEmis = teSVCRS) or
             (dmDanfe.NFe.Ide.tpEmis = teSCAN)) then
         begin
            //não exibe código de barras adicional
            wBarcode := FindRaveComponent('BarCode_Contigencia',wPage[1]) as TRaveCode128BarCode;
            if (wBarcode <> nil) then
               wBarCode.Left := 30;
         end
         else if ((dmDanfe.NFe.Ide.tpEmis = teContingencia) or
                  (dmDanfe.NFe.Ide.tpEmis = teFSDA)) then
         begin
            //não exibe textos
            wDataText[1]:=FindRaveComponent('DataText_DANFE1',wPage[1]) as TRaveDataText;
            wDataText[2]:=FindRaveComponent('DataText_DANFE2',wPage[1]) as TRaveDataText;
            if (wDataText[1] <> nil) then
               wDataText[1].Left := 30;
            if (wDataText[2] <> nil) then
               wDataText[2].Left := 30;
         end;

         //quadro fatura
         if (length(dmDanfe.NFe.Cobr.Fat.nFat) <= 0) then
         begin
            wPage[1] := FindRaveComponent('GlobalFatura',nil) as TRavePage;
            wText[1] := FindRaveComponent('Fatura_nFat_C',wPage[1]) as TRaveText;
            wDataText[1] := FindRaveComponent('Fatura_nFat',wPage[1]) as TRaveDataText;
            wText[2] := FindRaveComponent('Fatura_vOrig_C',wPage[1]) as TRaveText;
            wDataText[2] := FindRaveComponent('Fatura_vOrig',wPage[1]) as TRaveDataText;
            wText[3] := FindRaveComponent('Fatura_vDesc_C',wPage[1]) as TRaveText;
            wDataText[3] := FindRaveComponent('Fatura_vDesc',wPage[1]) as TRaveDataText;
            wText[4] := FindRaveComponent('Fatura_vLiq_C',wPage[1]) as TRaveText;
            wDataText[4] := FindRaveComponent('Fatura_vLiq',wPage[1]) as TRaveDataText;
            i:=4;
            while i>0 do
            begin
               if (wText[i] <> nil) then
                  wText[i].Left:=30;
               i:=i-1;
            end;
            i:=4;
            while i>0 do
            begin
               if (wDataText[i] <> nil) then
                  wDataText[i].Left:=30;
               i:=i-1;
            end;
         end;

         //omitir campos quadro volume (AGUARDANDO LEI QUE APROVE A MUDANÇA)
{         if dmDanfe.NFe.Transp.Vol.Count > 0 then
         begin
            wPage[1] := FindRaveComponent('GlobalTransportador',nil) as TRavePage;

            for I := 0 to dmDanfe.NFe.Transp.Vol.Count - 1 do
            begin
               wDataText[5] := FindRaveComponent('DataText5',wPage[1]) as TRaveDataText;
               wDataText[12] := FindRaveComponent('DataText12',wPage[1]) as TRaveDataText;
               wDataText[17] := FindRaveComponent('DataText17',wPage[1]) as TRaveDataText;

               if dmDanfe.NFe.Transp.Vol.Items[I].qVol=0 then
               begin
                  if (wDataText[5] <> nil) then
                     wDataText[5].Left:=30;
               end;
               if dmDanfe.NFe.Transp.Vol.Items[I].pesoB=0 then
               begin
                  if (wDataText[17] <> nil) then
                     wDataText[17].Left:=30;
               end;
               if dmDanfe.NFe.Transp.Vol.Items[I].pesoL=0 then
               begin
                  if (wDataText[12] <> nil) then
                     wDataText[12].Left:=30;
               end;
            end;
         end;}

         //Total2Liquido
         if FImprimirTotalLiquido then
         begin
            wPage[1] := FindRaveComponent('GlobalDadosProdutos',nil) as TRavePage;
            wText[1]:= FindRaveComponent('Text_vTotal',wPage[1]) as TRaveText;
            if (wText[1] <> nil) then
               wText[1].Text:='V.LÍQUIDO';
         end;

         //Casas Decimais (qCom)
         wDataView := FindRaveComponent('CustomDadosProdutosCX',nil) as TRaveDataView;
         wFloatField[1] := FindRaveComponent('CustomDadosProdutosCXQCom',wDataView) as TRaveFloatField;
         if (wFloatField[1] <> nil) then
         begin
            if (NaoEstaVazio(FCasasDecimais._Mask_qCom)) then
               wFloatField[1].DisplayFormat:=FCasasDecimais._Mask_qCom
            else
               wFloatField[1].DisplayFormat:=wDisplayFormat(FCasasDecimais._QCom);
         end;
         //Casas Decimais (vUnCom)
         wFloatField[2] := FindRaveComponent('CustomDadosProdutosCXVUnCom',wDataView) as TRaveFloatField;
         if (wFloatField[2] <> nil) then
         begin
            if (NaoEstaVazio(FCasasDecimais._Mask_vUnCom)) then
               wFloatField[2].DisplayFormat:=FCasasDecimais._Mask_vUnCom
            else
               wFloatField[2].DisplayFormat:=wDisplayFormat(FCasasDecimais._vUnCom);
         end;

         //NÃO imprime o LOGO se o mesmo não for especificado
         if EstaVazio(FLogo) then
         begin
            wPage[1] := FindRaveComponent('GlobalDANFE',nil) as TRavePage;
            wBitmap[1] := FindRaveComponent('Bitmap1',wPage[1]) as TRaveBitmap;
            if (wBitmap[1] <> nil) then
               wBitmap[1].Left:=30;
         end;

         //Bordas
         if (FEspessuraBorda <> 1) then
         begin
            wPage[1] := FindRaveComponent('GlobalRecibo',nil) as TRavePage;
            i:=2;
            while i>0 do
            begin
               if (i in [1,2]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wHLine[i] := FindRaveComponent('HLine'+inttostr(i),wPage[1]) as TRaveHLine;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wHLine[i] <> nil) then
                     wHLine[i].LineWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalDANFE',nil) as TRavePage;
            i:=7;
            while i>0 do
            begin
               if (i in [1,2,3,4,5,6,7]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wSquare[i] := FindRaveComponent('Square'+inttostr(i),wPage[1]) as TRaveSquare;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wSquare[i] <> nil) then
                     wSquare[i].BorderWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalDestinatario',nil) as TRavePage;
            i:=15;
            while i>0 do
            begin
               if (i in [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalFatura',nil) as TRavePage;
            i:=2;
            while i>0 do
            begin
               wRectangle[1] := FindRaveComponent('Rectangle_Fatura'+inttostr(i),wPage[1]) as TRaveRectangle;
               if (wRectangle[1] <> nil) then
                  wRectangle[1].BorderWidth:=FEspessuraBorda;
               if (i in [1,2]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalCalculoImposto',nil) as TRavePage;
            i:=9;
            while i>0 do
            begin
               if (i in [1,2,3,4,5,6,7,8,9]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalTransportador',nil) as TRavePage;
            i:=18;
            while i>0 do
            begin
               if (i in [1,2,3,4,5,6,7,8,14,15,16,17,18]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalDadosProdutos',nil) as TRavePage;
            i:=29;
            while i>0 do
            begin
               if (i in [1,2,3,4,5,6,7,8,9,10,11,12,13,29]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalCalculoISSQN',nil) as TRavePage;
            i:=6;
            while i>0 do
            begin
               if (i in [3,4,6]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wPage[1] := FindRaveComponent('GlobalDadosAdicionais',nil) as TRavePage;
            i:=6;
            while i>0 do
            begin
               if (i in [4,6]) then
               begin
                  wRectangle[i] := FindRaveComponent('Rectangle'+inttostr(i),wPage[1]) as TRaveRectangle;
                  wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[1]) as TRaveVLine;
                  if (wRectangle[i] <> nil) then
                     wRectangle[i].BorderWidth:=FEspessuraBorda;
                  if (wVLine[i] <> nil) then
                     wVLine[i].LineWidth:=FEspessuraBorda;
               end;
               i:=i-1;
            end;

            wReport := FindRaveComponent('DANFE1',nil) as TRaveReport;
            wPage[1] := FindRaveComponent('Page1',wReport) as TRavePage;
            wPage[2] := FindRaveComponent('Page2',wReport) as TRavePage;
            j:=2;
            while j>0 do
            begin
               if wPage[j] <> nil then
               begin
                  i:=32;
                  while i>0 do
                  begin
                     wVLine[i] := FindRaveComponent('VLine'+inttostr(i),wPage[j]) as TRaveVLine;
                     wHLine[i] := FindRaveComponent('HLine'+inttostr(i),wPage[j]) as TRaveHLine;
                     if (wVLine[i] <> nil) then
                        wVLine[i].LineWidth:=FEspessuraBorda;
                     if (wHLine[i] <> nil) then
                        wHLine[i].LineWidth:=FEspessuraBorda;
                     i:=i-1;
                  end;
               end;
               j:=j-1;
            end;
         end;

         //Margem Inferior
         wReport := FindRaveComponent('DANFE1',nil) as TRaveReport;
         wPage[1] := FindRaveComponent('Page1',wReport) as TRavePage;
         wDataText[1] := FindRaveComponent('DataText1',wPage[1]) as TRaveDataText;
         wDataText[2] := FindRaveComponent('DataText2',wPage[1]) as TRaveDataText;
         wPage[2] := FindRaveComponent('GlobalDadosAdicionais',nil) as TRavePage;
         wSection := FindRaveComponent('Section_DadosAdicionais',wPage[2]) as TRaveSection;
         wBand := nil;
         if (wSection = nil) then
            wBand:=FindRaveComponent('Band_ISSQNDadosAdicionais',wPage[1]) as TRaveBand;
         if (wDataText[1] <> nil) then
         begin
            vMargemInferiorAtual:=(wPage[1].PageHeight-wDataText[1].Top);
            vHeightPadrao:=wDataText[1].Height;
         end
         else
         begin
            vMargemInferiorAtual:=0.8/2.54;
            vHeightPadrao:=0;
         end;
         vMargemInferior := FMargemInferior/2.54;
         if (wDataText[1] <> nil) then
            wDataText[1].Top := wDataText[1].Top-vHeightPadrao-(vMargemInferior-vMargemInferiorAtual);
         if (wDataText[2] <> nil) then
            wDataText[2].Top := wDataText[2].Top-vHeightPadrao-(vMargemInferior-vMargemInferiorAtual);
         if (wSection <> nil) then
            wSection.Height := wSection.Height-vHeightPadrao-(vMargemInferior-vMargemInferiorAtual)
         else
         begin
            if (wBand <> nil) then
               wBand.Height := wBand.Height-(vMargemInferior-vMargemInferiorAtual);
         end;

         //Margem Superior
         wReport := FindRaveComponent('DANFE1',nil) as TRaveReport;
         wPage[1] := FindRaveComponent('Page1',wReport) as TRavePage;
         wPage[2] := FindRaveComponent('Page2',wReport) as TRavePage;
         wRegion[1] := FindRaveComponent('Region1',wPage[1]) as TRaveRegion;
         wRegion[2] := FindRaveComponent('Region1',wPage[2]) as TRaveRegion;
         if (wRegion[1] <> nil) then
            vMargemSuperiorAtual:=wRegion[1].Top
         else
            vMargemSuperiorAtual:=0.406/2.54;
         vMargemSuperior:=FMargemSuperior/2.54;
         vMargemSuperior:=vMargemSuperiorAtual-vMargemSuperior;
         i:=1;
         while (i <= 2) do
         begin
            if (wRegion[i] <> nil) then
            begin
               if (vMargemSuperior < 0) then
               begin
                  wRegion[i].Top:=wRegion[i].Top+(vMargemSuperior*(-1));
                  wRegion[i].Height:=wRegion[i].Height-(vMargemSuperior*(-1));
               end
               else if (vMargemSuperior > 0) then
               begin
                  wRegion[i].Top:=wRegion[i].Top-(vMargemSuperior);
                  wRegion[i].Height:=wRegion[i].Height+(vMargemSuperior);
               end;
            end;
            i:=i+1;
         end;
      end;
   finally
      dmDanfe.TributosPercentual := FtributosPercentual;
      dmDanfe.MarcaDaguaMSG := FMarcaDaguaMSG;
      dmDanfe.RvProject.ExecuteReport('DANFE1');
      dmDanfe.RvProject.Close;
      ProtocoloNFe:='';
   end;
end;

procedure TACBrNFeDANFERave.OnPrintRaveSystem(Sender: TObject);
var
  i: Integer;
begin
  with Sender as TBaseReport do
    for i:= 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count-1 do
    begin
      if ( i > 0 ) then
        NewPage;
      dmDanfe.NFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe;
      dmDanfe.RvSystem1.SystemPrinter.Title := TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe.infNFe.ID;
      // processo para nao exibir o quadro ISSQN no DANFE_Rave513
      if (  ( dmDanfe.NFe.Total.ISSQNtot.VServ = 0 )
        and ( dmDanfe.NFe.Total.ISSQNtot.VBC   = 0 )
        and ( dmDanfe.NFe.Total.ISSQNtot.VISS  = 0 ) ) then
        dmDanfe.RvProject.SetParam('wISSQN','N')
      else
        dmDanfe.RvProject.SetParam('wISSQN','S');
      ExecutaReport;
    end;
end;

procedure TACBrNFeDANFERave.ImprimirDANFE(NFE : TNFe = nil);
var
 wProjectStream: TStringStream;
begin
  {$IFDEF RAVE50VCL}
     RPDefine.DataID := IntToStr(Application.Handle);  // Evita msg de erro;...
  {$ENDIF}

   if FRaveFile = '' then
      raise EACBrNFeException.Create(' Arquivo de Relatório nao informado.') ;

   if not (uppercase(copy(FRaveFile,length(FRaveFile)-3,4))='.RAV') then
   begin
      wProjectStream:=TStringStream.Create(FRaveFile);
      dmDanfe.RvProject.ProjectFile := '';
      dmDanfe.RvProject.LoadRaveBlob(wProjectStream);
      wProjectStream.Free;
   end
   else
   begin
      if not FilesExists(FRaveFile) then
         raise EACBrNFeException.Create('Arquivo '+FRaveFile+' Nao encontrado');

      dmDanfe.RvProject.ClearRaveBlob;
      dmDanfe.RvProject.ProjectFile := FRaveFile;  //ExtractFileDir(application.ExeName)+'\Report\NotaFiscalEletronica.rav';
   end;

  dmDanfe.RvSystem1.DoNativeOutput := True;
  dmDanfe.RvRenderPDF1.Active := True;
  if MostrarPreview then
   begin
     dmDanfe.RvSystem1.DefaultDest    := rdPreview;
     dmDanfe.RvSystem1.SystemSetups:=dmDanfe.RvSystem1.SystemSetups + [ssAllowSetup];
   end
  else
   begin
     dmDanfe.RvSystem1.DefaultDest    := rdPrinter;
     dmDanfe.RvSystem1.SystemSetups:=dmDanfe.RvSystem1.SystemSetups - [ssAllowSetup];
   end;
  dmDanfe.RvSystem1.RenderObject   := nil;
  dmDanfe.RvSystem1.OutputFileName := '';
  dmDanfe.RvProject.Engine := dmDanfe.RvSystem1;
  dmDanfe.RvSystem1.TitlePreview:='Visualizar DANFE';
  dmDanfe.RvSystem1.TitleSetup:='Configurações';
  dmDanfe.RvSystem1.TitleStatus:='Status da Impressão';
  dmDanfe.RvSystem1.SystemFiler.StatusFormat:='Gerando página %p';
  dmDanfe.RvSystem1.SystemFiler.StreamMode:=smMemory;
  dmDanfe.RvSystem1.SystemOptions:=[soShowStatus,soAllowPrintFromPreview,soPreviewModal];
  if not MostrarStatus then
     dmDanfe.RvSystem1.SystemOptions:=dmDanfe.RvSystem1.SystemOptions - [soShowStatus];
  dmDanfe.RvSystem1.SystemPreview.FormState:=wsMaximized;
  dmDanfe.RvSystem1.SystemPreview.ZoomFactor:=100;
  dmDanfe.RvSystem1.SystemPrinter.Copies:=NumCopias;

  // DONE -oJacinto Junior: Ajustar a orientação de impressão conforme tipo do DANFe.
  if FTipoDANFE = tiRetrato then
    dmDanfe.RvSystem1.SystemPrinter.Orientation := poPortrait
  else if FTipoDANFE = tiPaisagem then
    dmDanfe.RvSystem1.SystemPrinter.Orientation := poLandScape;

  dmDanfe.RvSystem1.SystemPrinter.LinesPerInch:=8;
  dmDanfe.RvSystem1.SystemPrinter.StatusFormat:='Imprimindo página %p';
  dmDanfe.RvSystem1.SystemPrinter.Title:= 'NFe - Impressão do DANFE';
  dmDanfe.RvSystem1.SystemPrinter.Units:=unMM;

  {$IFDEF RaveCollate}
    dmDanfe.RvSystem1.SystemPrinter.Collate:=true;
  {$ENDIF}

  dmDanfe.RvSystem1.SystemPrinter.UnitsFactor:=25.4;

  if Length(Impressora) > 0 then
     RpDev.SelectPrinter(Impressora, false);
  //dmDanfe.RvSystem1.BaseReport.SelectPrinter(Impressora);
  if NFE = nil then
  begin
    try
      dmDanfe.RvSystem1.OnBeforePrint := nil;
      dmDanfe.RvSystem1.OnPrint := OnPrintRaveSystem;
      dmDanfe.RvSystem1.SystemPrinter.Units := unInch;
      dmDanfe.RvSystem1.SystemPrinter.UnitsFactor := 1;
      dmDanfe.RvSystem1.Execute;
    finally
      dmDanfe.RvSystem1.OnBeforePrint := dmDanfe.RvSystem1BeforePrint;
      dmDanfe.RvSystem1.OnPrint := nil;
    end;
  end
  else
  begin
    dmDanfe.NFe := NFE;
    ExecutaReport;
  end;
end;

procedure TACBrNFeDANFERave.ImprimirDANFEPDF(NFE : TNFe = nil);
var
 i : Integer;
 NomeArq : String;
 wProjectStream: TStringStream;
begin
  {$IFDEF RAVE50VCL}
     RPDefine.DataID := IntToStr(Application.Handle);  // Evita msg de erro;...
  {$ENDIF}

   if FRaveFile = '' then
      raise EACBrNFeException.Create(' Arquivo de Relatório nao informado.') ;

   if not (uppercase(copy(FRaveFile,length(FRaveFile)-3,4))='.RAV') then
   begin
      wProjectStream:=TStringStream.Create(FRaveFile);
      dmDanfe.RvProject.ProjectFile := '';
      dmDanfe.RvProject.LoadRaveBlob(wProjectStream);
      wProjectStream.Free;
   end
   else
   begin
      if not FilesExists(FRaveFile) then
         raise EACBrNFeException.Create('Arquivo '+FRaveFile+' Nao encontrado');

      dmDanfe.RvProject.ClearRaveBlob;
      dmDanfe.RvProject.ProjectFile := FRaveFile;  //ExtractFileDir(application.ExeName)+'\Report\NotaFiscalEletronica.rav';
   end;

  dmDanfe.RvSystem1.DefaultDest := rdFile;
  dmDanfe.RvSystem1.DoNativeOutput:=false;

  {$IFDEF RaveCollate}
    dmDanfe.RvSystem1.SystemPrinter.Collate:=true;
  {$ENDIF}

  dmDanfe.RvSystem1.RenderObject:= dmDanfe.RvRenderPDF1;
  if not MostrarStatus then
     dmDanfe.RvSystem1.SystemOptions:=dmDanfe.RvSystem1.SystemOptions - [soShowStatus];
  dmDanfe.RvSystem1.SystemSetups:=dmDanfe.RvSystem1.SystemSetups - [ssAllowSetup];
  dmDanfe.RvProject.Engine := dmDanfe.RvSystem1;
  dmDanfe.RvRenderPDF1.EmbedFonts:=False;
  dmDanfe.RvRenderPDF1.ImageQuality:=90;
  dmDanfe.RvRenderPDF1.MetafileDPI:=300;
  dmDanfe.RvRenderPDF1.UseCompression:=False;
  dmDanfe.RvRenderPDF1.Active:=True;

   if NFE = nil then
   begin
      for i:= 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count-1 do
      begin
         dmDanfe.NFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe;

         NomeArq := StringReplace(TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]);
         NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'-nfe.pdf';

         dmDanfe.RvSystem1.OutputFileName := NomeArq;
         ExecutaReport;
      end;
   end
   else
   begin
      dmDanfe.NFe := NFE;
      NomeArq := StringReplace(NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]);
      NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'-nfe.pdf';

      dmDanfe.RvSystem1.OutputFileName := NomeArq;
      ExecutaReport;
   end;

  dmDanfe.RvRenderPDF1.Active:=False;
end;


end.
