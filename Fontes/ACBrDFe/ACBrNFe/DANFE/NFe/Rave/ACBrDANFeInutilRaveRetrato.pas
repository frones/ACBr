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
|* 06/08/2014: João Paulo da Silva Leão
|*  - Impressão do comprovante de Inutilização da Numeração
|*  - Adaptação da rotina de impressão de evento, para esta utilidade
******************************************************************************}
{$I ACBr.inc}

unit ACBrDANFeInutilRaveRetrato;

interface

uses Graphics, Forms, Windows, SysUtils, Classes,
     Variants, Math, StdCtrls, DB, Dialogs,
     Controls, ExtCtrls, Mask, MaskUtils,
     {$IFNDEF COMPILER16} JPEG, {$ELSE} Vcl.Imaging.jpeg, {$ENDIF}
     RpDefine, RpBase, RpSystem, RpBars, RpMemo,
     RpRenderText, RpRenderRTF, RpRenderHTML, RpRender, RpRenderPDF,
     ACBrNFe, pcnConversao, ACBrDANFeCBRave, ACBrDFeUtil;

const aHeigthPadrao:Double=5.8;

procedure ImprimirInutilRetrato(aRaveSystem:TInutilizacaoRave);

implementation

uses pcnInutNFe;

procedure ImprimirInutilRetrato(aRaveSystem:TInutilizacaoRave);
var
 PosX, PosY, PosBarra : Double;
 aWidth, CenterX, YY : Double;
 FMemo : TMemoBuf;
begin
  InutilizacaoRave:=aRaveSystem;

  // marca d'agua de homologação ***********************************************
  with InutilizacaoRave, InutilizacaoRave.BaseReport, InutilizacaoRave.ACBrNFe.InutNFe.InutNFe do
  begin
    if ( tpAmb = taHomologacao ) then
     begin
        YY :=FLastY-15;
        SetFont(FontNameUsed,25);
        FontColor:=clSilver;
        Bold:=True;
        Underline:=True;
        GotoXY(FFirstX+5,YY);
        FontRotation:=30;
        Print('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
     end;

     PosX := 10;
     PosY := 10;
     aWidth := 190;

     Box([],PosX,PosY,aWidth,18,'');
     SetFont(FontNameUsed,12);
     CenterX:=PosX+(aWidth/2);
     NewLine;
     Bold:=True;
     PrintCenter('INUTILIZAÇÃO DE NUMERAÇÃO DA NF-e',CenterX);
     SetFont(FontNameUsed,10);
     NewLine;
     Bold:=False;
     PrintCenter('Não possui valor fiscal, simples representação do fato indicado abaixo.',CenterX);
     NewLine;
     PrintCenter('CONSULTE A AUTENTICIDADE NO SITE DA SEFAZ AUTORIZADORA.',CenterX);
     NewLine;
     NewLine;
     PosY := YPos + 2;
     PosX := 10;
     SetFont(FontNameUsed,10);
     Box([],PosX,PosY, 12,aHeigthPadrao,'MODELO', IntToStr(modelo) ,taCenter);
     Box([],XPos,YPos, 12,aHeigthPadrao,'SÉRIE', IntToStr(serie),taCenter);
     Box([],XPos,YPos, 43,aHeigthPadrao,'NÚMERAÇÃO',IntToStr(nNFIni)+' a '+IntToStr(nNFFin),taCenter);
     Box([],XPos,YPos, 43,aHeigthPadrao,'ANO',IntToStr(ano),taCenter);

     PosBarra := XPos+41;
     Box([],XPos,PosY, 80,aHeigthPadrao*2) ;
     with TRPBarsCode128.Create(InutilizacaoRave.BaseReport) do
      begin
        BaseReport:=InutilizacaoRave.BaseReport;
        CodePage:=cpCodeC;
        BarCodeJustify:=pjCenter;
        UseChecksum:=false;
        BarWidth:=0.254;
        BarHeight:=10.0;
        WideFactor:=BarWidth;
        PrintReadable:=False;
        Text:=OnlyNumber(nProt);
        PrintXY(PosBarra,PosY+1);
        Free;
     end;

     PosY := YPos + aHeigthPadrao;
     PosX := 10;
     if (tpAmb = taProducao) then
        Box([],PosX,PosY,109.8,aHeigthPadrao,'AMBIENTE','PRODUÇÃO',taLeftJustify)
       else
        Box([],PosX,PosY,109.8,aHeigthPadrao,'AMBIENTE','HOMOLOGAÇÃO - SEM VALOR FISCAL',taLeftJustify);
     //NewLine;
     //NewLine;
     //PosY := YPos + 2;
     SetFont(FontNameUsed,10);
     PosY := YPos + aHeigthPadrao;
     Box([],PosX,PosY,110,aHeigthPadrao,'STATUS', IntToStr(cStat)+' - '+xMotivo,taLeftJustify);
     Box([],XPos,YPos, 38,aHeigthPadrao,'PROTOCOLO', nProt,taLeftJustify);
     Box([],XPos,YPos, 42,aHeigthPadrao,'DATA E HORÁRIO DO REGISTRO',DateTimeToStr(dhRecbto),taRightJustify);
     PosY := YPos + aHeigthPadrao;
     Box([],PosX,PosY,190,aHeigthPadrao,'JUSTIFICATIVA', xJust,taLeftJustify);

     SetFontTitle;
     PrintXY(PosX,FLastY,'DATA E HORA DA IMPRESSÃO: '+FormatDateTime('dd/mm/yyyy hh:mm:ss',Now)+IfThen((Trim(NomeDoUsuario)<>''),' - '+NomeDoUsuario,''));
     if Trim(NomeDoERP)>'' then
      begin
        PrintRight('Desenvolvido por '+NomeDoERP,FLastX-5);
      end;
  end;

end;


end.

