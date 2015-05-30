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
|* 20/08/2009: João Paulo
|*  - Doação units para geração do Danfe via código usando Rave
******************************************************************************}
{$I ACBr.inc}
unit ACBrDANFeCBRavePaisagem;

interface

uses Graphics, Forms, Windows, SysUtils, Classes,
     Variants, DBClient, Math, StdCtrls, DB, Dialogs,
     Controls, ExtCtrls, Mask, MaskUtils, DateUtils,
     {$IFNDEF COMPILER16} JPEG, {$ELSE} Vcl.Imaging.jpeg, {$ENDIF}
     RpDefine, RpBase, RpSystem, RpBars, RpMemo,
     RpRenderText, RpRenderRTF, RpRenderHTML, RpRender, RpRenderPDF,
     ACBrNFe, pcnConversao, ACBrDANFeCBRave;

const aWidthTituloBloco:Double=6;
      aDiferentHeigth:Double=0.08;
      aDiferentWidth:Double=0.08;
      FontSizeGroup:Integer=5;
      FontSizeTitle:Integer=6;
//      FontSizeText:Integer=10;

var
   FontSizeText:Double;
   ColsWidth:array[1..18] of Double; //colunas dados dos produtos


procedure ImprimirPaisagem(aRaveSystem:TDANFeRave);

implementation

uses ACBrNFeUtil, ACBrDFeUtil, StrUtils, pcnNFe;

function ImprimirCanhoto(PosX,PosY:Double):Double;
var aHeigthNumSerie, aHeigthIdent,
    aWidth, aWidthReceb: Double;
    vEnd: string;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     aHeigthNumSerie:=35;
     aHeigthIdent:=122;
     aWidth:=16;
     aWidthReceb:=8;
     if (FPageNum=1) then
      begin
        if (not FormularioContinuo) then
        begin
          Box([],PosX,PosY,aWidth,aHeigthNumSerie,'','',taLeftJustify,True,False,False); //numero e serie da nfe
          Box([fsTop],PosX,YPos,aWidthReceb,FLastY-YPos); //Informações sobre a nota no recebemos
          Box([fsTop,fsBottom],PosX+aWidthReceb,PosY+aHeigthNumSerie,aWidth-aWidthReceb,aHeigthIdent,'','',taLeftJustify,True,False,False); //identificação e assinatura
          Box([fsLeft],PosX+aWidthReceb,YPos,aWidth-aWidthReceb,FLastY-YPos); //data recebimento
          SetPen(FColorBorders,psDot,EspessuraBorda,pmCopy);
          if PosicaoCanhoto = 0 then
          begin
            MoveTo(PosX+aWidth+3,FFirstY);
            LineTo(PosX+aWidth+3,FLastY);
          end
          else
          begin
            MoveTo(PosX-3,FFirstY);
            LineTo(PosX-3,FLastY);
          end;
          SetPen(FColorBorders,psSolid,EspessuraBorda,pmCopy);
        end;

        SetFont(FontNameUsed,8);
        Bold:=True;
        GotoXY(PosX,PosY);
        NewLine;
        PrintCenter('NOTA',PosX+8);
        NewLine;
        PrintCenter('FISCAL',PosX+8);
        NewLine;
        PrintCenter('Nº',PosX+8);
        FontRotation:=90;
        PrintXY(PosX+1.5+((aWidth-LineHeight)/2),PosY+33,FNumeroNF);
        FontRotation:=90;
        PrintXY(PosX+LineHeight+1.5+((aWidth-LineHeight)/2),PosY+33,'Série: '+IntToStr(Ide.Serie));

        if (not FormularioContinuo) then
        begin
          SetFontTitle;
          FontRotation:=90;
          Bold:=True;
          GotoXY(PosX+FontHeight+0.5,FLastY-2);
          //GotoXY(PosX+LineHeight,FLastY-2);
          vEnd:='Recebemos de '+Emit.XNome+' os produtos / serviços constantes da Nota Fiscal indicada ao lado';
          if Length(vEnd)>122 then
          begin
             vEnd:='Recebemos de '+Emit.XNome;
             Print(vEnd);
             GotoXY(PosX+FontHeight+FontHeight+0.5,FLastY-2);
             vEnd:='os produtos / serviços constantes da Nota Fiscal indicada ao lado';
             Print(vEnd);
          end
          else
          begin
             Print(vEnd);
          end;

             if ExibirResumoCanhoto then
             begin
                GotoXY(PosX+2.5+FontHeight+FontHeight+0.5,FLastY-2);
                if EstaVazio(ExibirResumoCanhoto_Texto) then
                   Print('Emissão:'+FormatDate(DateToStr(Ide.DEmi))+' Dest/Rem:'+Dest.XNome+' Total:'+FormatFloat(Total.ICMSTot.VNF))
                else
                   Print(ExibirResumoCanhoto_Texto);
             end;

          Bold:=False;
          GotoXY(PosX+aWidthReceb+LineHeight,PosY+aHeigthNumSerie+aHeigthIdent-1);
          Print('Identificação e Assinatura do Recebedor');
          GotoXY(XPos,FLastY-2);
          Print('Data de Recebimento');
          FontRotation:=00;
        end;
     end;

    if PosicaoCanhoto = 0 then
      Result:=PosX+aWidth+6
    else
      Result:=PosX-6;
   end;
end;

procedure ImprimirMensagensDeFundo(PosX:Double);
var CenterX,YY:Double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     YY:=FLastY-5;
     if Ide.TpAmb=taHomologacao then
      begin //homologação
        SetFont(FontNameUsed,28);
        FontColor:=clSilver;
        Bold:=True;
        Underline:=True;
        GotoXY(PosX+5,YY);
        FontRotation:=33;
        Print('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
      end
     else if ((procNFe.cStat in [101,151]) or (NFeCancelada)) then
      begin // cancelada
        SetFont(FontNameUsed,28);
        FontColor:=clRed;
        Bold:=True;
        Underline:=True;
        GotoXY(PosX+5,YY);
        FontRotation:=33;
        Print('NFe Cancelada');
      end
     else if (procNFe.cStat = 110) then
      begin // denegada
        SetFont(FontNameUsed,28);
        FontColor:=clRed;
        Bold:=True;
        Underline:=True;
        GotoXY(PosX+5,YY);
        FontRotation:=33;
        Print('NFe Denegada');
      end
     else if ((procNFe.cStat <> 100 ) and
              (Ide.tpEmis <> teFSDA) and
              (Ide.tpEmis <> teCONTINGENCIA) and
              (Length(Trim(ProtocoloNFe)) < 15)) then
      begin //Não autorizada
        SetFont(FontNameUsed,28);
        FontColor:=clRed;
        Bold:=True;
        Underline:=True;
        GotoXY(PosX+5,YY);
        FontRotation:=33;
        if procNFe.cStat <= 0 then
           Print('NFe não autorizada pela SEFAZ(SEM VALIDADE FISCAL)')
        else
           Print(IntToStr(procNFe.cStat) + '-' +procNFe.xMotivo);
      end;

      //Marca Dagua
      begin
        SetFont(FontNameUsed,25);
        FontColor:=clSilver;
        Bold:=True;
        Underline:=True;
        GotoXY(FFirstX+15,PageHeight / 2);
        //GotoXY(PosX+15,YY+10);
        FontRotation:=33;
        CenterX:=XPos+((PageWidth-MarginRight-XPos)/2);
        PrintCenter(MarcaDaguaMSG,CenterX);
      end;


     SetFont(FontNameUsed,22);
     FontColor:=clSilver;
     Bold:=True;
     GotoXY(PosX+5,YY-10);
     CenterX:=XPos+((PageWidth-MarginRight-XPos)/2);
     case Ide.tpEmis of
        teFSDA,
        teContingencia,
        teSVCAN,
        teSVCRS,
        teSCAN:
                begin
                  PrintCenter('DANFE em Contingência - impresso em',CenterX);
                  NewLine;
                  PrintCenter('decorrência de problemas técnicos',CenterX);
                end;
     end;

     FontRotation:=0;
   end;
end;

function ImprimirEmitente(PosX,PosY:Double):Double;
   //função para dividir texto - Créditos: Luis
   function DivideTexto( var vTexto:string; aMax:integer ):string;
   var
    vPalavra,vRetorno:string;
   begin
    vRetorno := '';
    while length(vTexto)>aMax do
    begin
       vPalavra := RightStr(vTexto,1);
       vTexto   := Copy( vTexto, 1, Length(vTexto)-1 );
       while ( length(vTexto) > 0 ) and ( RightStr(vTexto,1) <> ' ' ) and ( RightStr(vTexto,1) <> '.' ) do
       begin
          vPalavra := RightStr(vTexto,1)+vPalavra;
          vTexto   := Copy( vTexto, 1, Length(vTexto)-1 );
       end;
       vRetorno := vPalavra + vRetorno;
    end;
    if length(vRetorno)>aMax then vRetorno := Copy( vRetorno, 1, aMax );
    Result := vRetorno;
   end;
var aHeigthLogo, aWidthLogo, aWidth,CenterX:Double;
    aWidthTexto: integer;
    stLogo:TMemoryStream;
    vEnd,vTemp:String;
    vDuasLinhas: boolean;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
    stLogo:=TMemoryStream.Create;
    try
       if LogoMarca<>nil then
          LogoMarca.SaveToStream(stLogo);
       stLogo.Position:=0;
       aWidth:=106;
       Result:=PosX+aWidth;
       if (FormularioContinuo) then
          exit;

       Box([],PosX,PosY,aWidth,30,'IDENTIFICAÇÃO DO EMITENTE');

       if (ExpandirLogoMarca) and (Assigned(LogoMarca)) then
       begin
         PrintImageRect(PosX+1,PosY+3,PosX+aWidth-1,PosY+29,stLogo,'JPG');
       end
       else begin
         GotoXY(PosX,PosY+2);
         CenterX:=PosX+(aWidth/2);
         SetFont(FontNameUsed,FontSizeEmit_Nome);
         NewLine;
         Bold:=True;
         vEnd:=Emit.XNome;
         vDuasLinhas:=false;
         if length(vEnd)>38 then
         begin
           vtemp := DivideTexto(vEnd,38);
           vDuasLinhas:=true;
         end
         else
           vTemp:='';
         PrintCenter(vEnd,CenterX);
         if Length(Vtemp)>0 then
         begin
            NewLine;
            PrintCenter(vTemp,CenterX);
         end;
         GotoXY(PosX,YPos+2);

         aWidthLogo:=0;
         aWidthTexto:=64;
         if Assigned(LogoMarca) then
         begin
           if vDuasLinhas then
           begin
              aWidthLogo:=26-5;
              aHeigthLogo:=20-5
           end
           else
           begin
              aWidthLogo:=26;
              aHeigthLogo:=20;
           end;
           aWidthTexto:=54;
           PrintImageRect(PosX+1,YPos,PosX+aWidthLogo,YPos+aHeigthLogo,stLogo,'JPG');
         end;
         GotoXY(PosX,YPos+1.5);
         CenterX:=PosX+aWidthLogo+((aWidth-aWidthLogo)/2);
         SetFont(FontNameUsed,FontSizeEmit_Outros);
         Bold:=True;
         with Emit.EnderEmit do
          begin
           vEnd:=XLgr;
           if (Trim(Nro)>'') and (Nro<>'SN') then
              vEnd:=vEnd+' '+Nro;
           if Trim(XCpl)>'' then
              vEnd:=vEnd+', '+XCpl;
           if length(vEnd)>aWidthTexto then
              vtemp := DivideTexto(vEnd,aWidthTexto)
           else
              vTemp:='';
           PrintCenter(vEnd,CenterX);
           NewLine;
           vEnd:=vTemp+XBairro+' - '+NotaUtil.FormatarCEP(Poem_Zeros(CEP,8));
           PrintCenter(vEnd,CenterX);
           NewLine;
           vEnd:=XMun+' - '+UF;
           PrintCenter(vEnd,CenterX);
           NewLine;
           vEnd:='FONE: '+NotaUtil.FormatarFone(Fone);
           if trim(FaxDoEmitente)>'' then
              vEnd:=vEnd+' / FAX: '+NotaUtil.FormatarFone(FaxDoEmitente);
           PrintCenter(vEnd,CenterX);
           NewLine;
           if vDuasLinhas then
           begin
              vEnd:='';
              if (SiteDoEmitente <> '') and (EmailDoEmitente <> '') then
                 vEnd := SiteDoEmitente+' - '+EmailDoEmitente;
              if (SiteDoEmitente = '') and (EmailDoEmitente <> '') then
                 vEnd := EmailDoEmitente;
              PrintCenter(vEnd,CenterX);
           end
           else
           begin
              vEnd:=SiteDoEmitente;
              PrintCenter(vEnd,CenterX);
              NewLine;
              vEnd:=EmailDoEmitente;
              PrintCenter(vEnd,CenterX);
           end;
           Bold:=False;
          end;
       end;
    finally
      FreeAndNil(stLogo);
    end;
   end;
end;

function ImprimirTituloDANFe(PosX, PosY: Double):Double;
var aWidth, CenterX:Double;
    VarNumPage:String;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     aWidth:=41;
     Result:=PosX+aWidth;
     Box([fsLeft],PosX,PosY,aWidth,30);
     CenterX:=PosX+20;
     GotoXY(PosX,PosY);
     SetFont(FontNameUsed,FontSizeIdentDoc_DANFE);
     Bold:=True;
     NewLine;
     GotoXY(PosX,YPos-0.4);
     if (not FormularioContinuo) then
        PrintCenter('DANFE',CenterX);
     SetFont(FontNameUsed,FontSizeIdentDoc_TipoOperacao);
     NewLine;
     if (not FormularioContinuo) then
        PrintCenter('Documento Auxiliar da',CenterX);
     NewLine;
     if (not FormularioContinuo) then
        PrintCenter('Nota Fiscal Eletrônica',CenterX);
     NewLine;
     if (not FormularioContinuo) then
        PrintXY(PosX+10,YPos+0.5,'0 - ENTRADA');
     NewLine;
     if (not FormularioContinuo) then
        PrintXY(PosX+10,YPos+0.5,'1 - SAÍDA');
     Box([],PosX+30,PosY+14.5,4,4);
     SetFont(FontNameUsed,FontSizeIdentDoc_Outros);
     Bold:=True;
     PrintXY(PosX+31,PosY+17.8,IfThen(Ide.TpNF=tnEntrada,'0','1'));
     GotoXY(PosX,PosY+20.4);
     NewLine;
     PrintCenter('N.º '+FNumeroNF,CenterX);
     NewLine;

     VarNumPage:='PAGE'+FormatFloat('000000',FCurrentPage);

     PrintCenter('SÉRIE '+IntToStr(Ide.Serie)+' - FOLHA '+PIVar(VarNumPage),CenterX);
     Bold:=False;
   end;
end;

function ImprimirCodigoBarras(PosX, PosY: Double):Double;
var PosYCodBarraContigencia, aWidth, CenterX:Double;
    w1,w2,aTituloChave, aChaveAcesso, aProtocolo, aChaveContigencia:String;
begin
   with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
      aWidth:=FLastX-PosX;
      Box([fsLeft],PosX,PosY,aWidth,12.27,'','',taLeftJustify,True);
      if FEspelho then
         aChaveAcesso:='SEM VALOR FISCAL (SOMENTE CONFERENCIA)'
      else
         aChaveAcesso:=NotaUtil.FormatarChaveAcesso(FChaveNFe);
      if (not FormularioContinuo) then
        aTituloChave:='CHAVE DE ACESSO'
       else
        aTituloChave:=' ';
      Box([fsLeft,fsTop],PosX,YPos,aWidth,aHeigthPadrao,aTituloChave,aChaveAcesso,taCenter,True);

      if ACBrNFe.NotasFiscais.Items[FNFIndex].NFe.Ide.tpEmis in [teContingencia, teFSDA, teSVCAN, teSVCRS] then
         aChaveContigencia:=NotaUtil.GerarChaveContingencia(ACBrNFe.NotasFiscais.Items[FNFIndex].NFe)
      else
         aChaveContigencia:='';

      PosYCodBarraContigencia:=YPos;
      Box([fsLeft,fsTop],PosX,YPos,aWidth,12.27,'','',taLeftJustify,True);
      Result:=YPos;
      if aChaveContigencia<>'' then
         Box([fsLeft,fsTop],PosX,YPos,aWidth,aHeigthPadrao,'DADOS DA NFe',NotaUtil.FormatarChaveContigencia(aChaveContigencia),taCenter,True)
      else
      begin
         aProtocolo := ProtocoloNFe;
         if (ACBrNFe.NotasFiscais.Items[FNFIndex].NFe.Ide.tpEmis in [teNormal,teSCAN,teSVCAN,teSVCRS]) then  
         begin
            if EstaVazio(aProtocolo) then
               aProtocolo:=Trim(procNFe.nProt)+' '+IfThen(procNFe.dhRecbto<>0,DateTimeToStr(procNFe.dhRecbto),'');
            if ((NFeCancelada) or
                (ACBrNFe.NotasFiscais.Items[FNFIndex].NFe.procNFe.cStat in [101,151])) then
               Box([fsLeft,fsTop],PosX,YPos,aWidth,aHeigthPadrao,'PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO',aProtocolo,taCenter,True)
            else
            if (ACBrNFe.NotasFiscais.Items[FNFIndex].NFe.procNFe.cStat=110) then
               Box([fsLeft,fsTop],PosX,YPos,aWidth,aHeigthPadrao,'PROTOCOLO DE DENEGAÇÃO',aProtocolo,taCenter,True)
            else
               Box([fsLeft,fsTop],PosX,YPos,aWidth,aHeigthPadrao,'PROTOCOLO DE AUTORIZAÇÃO DE USO',aProtocolo,taCenter,True);
         end;
      end;

      if not FEspelho then
      begin
         with TRPBarsCode128.Create(DANFeRave.BaseReport) do
         begin
            BaseReport:=DANFeRave.BaseReport;
            CodePage:=cpCodeC;
            BarCodeJustify:=pjCenter;
            UseChecksum:=false;
            BarWidth:=0.39;
            BarHeight:=10.0;
            WideFactor:=BarWidth;
            PrintReadable:=False;
            Text:=OnlyNumber(FChaveNFe);
            PrintXY(PosX+(aWidth/2),PosY+1);
            Free;
         end;

         if aChaveContigencia<>'' then
         begin
            with TRPBarsCode128.Create(DANFeRave.BaseReport) do
            begin
               BaseReport:=DANFeRave.BaseReport;
               CodePage:=cpCodeC;
               BarCodeJustify:=pjCenter;
               UseChecksum:=false;
               BarWidth:=0.39;
               BarHeight:=10.0;
               WideFactor:=BarWidth;
               PrintReadable:=False;
               Text:=OnlyNumber(aChaveContigencia);
               PrintXY(PosX+(aWidth/2),PosYCodBarraContigencia+1);
               Free;
            end;
         end
         else
         begin
            SetFont(FontNameUsed,IfThen(Pos('Courier',FontNameUsed)>0,8,11));
            if OnlyNumber(aProtocolo)='' then
            begin
              Bold:=True;
              FontColor:=clRed;
              w1:='N F E   A I N D A   N Ã O   F O I   A U T O R I Z A D A';
              w2:='P E L A   S E F A Z  (SEM VALIDADE FISCAL)';
            end
            else
            begin
              w1:='Consulta de autenticidade no portal nacional da NF-e';
              w2:='www.nfe.fazenda.gov.br/portal ou no site da Sefaz Autorizadora';
            end;
            GotoXY(PosX,PosYCodBarraContigencia+5);
            CenterX:=PosX+(aWidth/2);

            PrintCenter(w1,CenterX);
            NewLine;
            PrintCenter(w2,CenterX);
            Bold:=False;
            FontColor:=clBlack;
         end;
      end
      else
      begin
         SetFont(FontNameUsed,24);
         Bold:=True;
         Underline:=True;
         GotoXY(PosX+1,PosY);
         NewLine;
         PrintXY(PosX+5,YPos-1,'SEM VALOR FISCAL');
         Bold:=False;
         Underline:=False;
      end;
   end;
end;


function ImprimirEmitenteOutrosDados(PosX,
  PosY, WidthNaturezaOperacao: Double): Double;
var wTemp:String;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     //PosX:=PosX+aWidthTituloBloco;
     if (not FormularioContinuo) then
        wTemp:='NATUREZA DA OPERAÇÃO'
       else
        wTemp:=' ';
     Box([fsTop],PosX,PosY,WidthNaturezaOperacao,aHeigthPadrao,wTemp,ide.natOp,taLeftJustify,True,False,False);
     if (not FormularioContinuo) then
     begin
       Box([fsTop],PosX,YPos,82,aHeigthPadrao,'INSCRIÇÃO ESTADUAL',Emit.IE,taLeftJustify);
       Box([fsTop,fsLeft],XPos,YPos,82,aHeigthPadrao,'INSCRIÇÃO ESTADUAL DO SUBST. TRIBUTÁRIO',Emit.IEST,taLeftJustify);
       Box([fsTop,fsLeft],XPos,YPos,87,aHeigthPadrao,'C.N.P.J.',FormatarCNPJ(Emit.CNPJCPF),taLeftJustify,True);
     end
     else begin
       Box([fsTop,fsLeft],XPos,YPos,87,aHeigthPadrao,' ','',taLeftJustify,True);
     end;
     Result:=YPos;
   end;
end;

procedure TituloDoBloco(FlagsHideLine:TFlagsShowLine; Y1, X2, Y2: Double;
  aDescricao: string;aDescricaoAux:string='');
var X1,CenterX,CenterY:Double;
begin
  with DANFeRave, DANFeRave.BaseReport do
   begin
    Y2:=Y2;
    X1:=X2-aWidthTituloBloco;
    Box(FlagsHideLine+[fsRigth],X1,Y1,X2-X1,Y2-Y1);
    SetFont(FontNameUsed,FontSizeGroup);
    Bold:=True;
    FontRotation:=90;
    FontColor:=clWhite;
    GotoXY(X1+2,Y1+2);
    Print('a'); //usado somente para pegar a altura da linha
    FontColor:=clBlack;
    CenterX:=X2-X1-FontHeight;
    if aDescricaoAux<>'' then
       CenterX:=CenterX-FontHeight;
    CenterX:=X1+CenterX;
    CenterY:=Y1+((Y2-Y1)/2);
    GotoXY(CenterX,CenterY);
    PrintCenter(aDescricao,CenterX);
    if aDescricaoAux<>'' then
     begin
       CenterX:=CenterX+FontHeight;
       GotoXY(CenterX,CenterY);
       PrintCenter(aDescricaoAux,CenterX);
     end;
   end;
end;

function ImprimirRemetenteDestinatario(PosX,
  PosY: Double): Double;
var vEnd:String; vEntSai: string;
vSaiEnt :string;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
      if ide.tpNF=tnEntrada then
         vEntSai:='Entrada'
      else
         vEntSai:='Saida';

     PosX:=PosX+aWidthTituloBloco;
     if FormularioContinuo then
        Box([],PosX,PosY,192,aWidthTituloBloco,'Nome / Razão Social',Dest.XNome)
     else
        Box([fsTop],PosX,PosY,192,aWidthTituloBloco,'Nome / Razão Social',Dest.XNome);
     if Length(Trim(Dest.CNPJCPF)) > 11 then
     begin
       if FormularioContinuo then
          Box([fsLeft],XPos,YPos,41,aWidthTituloBloco,'CNPJ / CPF',FormatarCNPJ(Dest.CNPJCPF),taCenter)
       else
          Box([fsTop,fsLeft],XPos,YPos,41,aWidthTituloBloco,'CNPJ / CPF',FormatarCNPJ(Dest.CNPJCPF),taCenter);
     end
     else
     begin
       if FormularioContinuo then
          Box([fsLeft],XPos,YPos,41,aWidthTituloBloco,'CNPJ / CPF',FormatarCPF(Dest.CNPJCPF),taCenter)
       else
          Box([fsTop,fsLeft],XPos,YPos,41,aWidthTituloBloco,'CNPJ / CPF',FormatarCPF(Dest.CNPJCPF),taCenter);
     end;
     if FormularioContinuo then
        Box([fsLeft],XPos,YPos,21,aWidthTituloBloco,'Data de Emissão',FormatDate(DateToStr(Ide.DEmi)),taCenter,True)
     else
        Box([fsTop,fsLeft],XPos,YPos,21,aWidthTituloBloco,'Data de Emissão',FormatDate(DateToStr(Ide.DEmi)),taCenter,True);
     with Dest.EnderDest do
      begin
       vEnd:=XLgr;
       if (Trim(Nro)>'') and (Nro<>'SN') then
          vEnd:=vEnd+' '+Nro;
       if Trim(XCpl)>'' then
          vEnd:=vEnd+', '+XCpl;
       Box([fsTop],PosX,YPos,136,aWidthTituloBloco,'Endereço',vEnd);
       Box([fsTop,fsLeft],XPos,YPos,56,aWidthTituloBloco,'Bairro',XBairro);
       Box([fsTop,fsLeft],XPos,YPos,38,aWidthTituloBloco,'CEP',NotaUtil.FormatarCEP(Poem_Zeros(CEP,8)),taCenter);
       Box([fsTop,fsLeft],XPos,YPos,21,aWidthTituloBloco,'Data de '+vEntSai,FormatDate(DateOf(Ide.DSaiEnt)),taCenter,True);
       Box([fsTop],PosX,YPos,136,aWidthTituloBloco,'Município',XMun);
       Box([fsTop,fsLeft],XPos,YPos,43,aWidthTituloBloco,'Fone / Fax',NotaUtil.FormatarFone(FONE),taCenter);
       Box([fsTop,fsLeft],XPos,YPos,13,aWidthTituloBloco,'Estado',UF,taCenter);
       Box([fsTop,fsLeft],XPos,YPos,38,aWidthTituloBloco,'Inscrição Estadual',Dest.IE,taCenter);

//       if ide.hSaiEnt=0 then
//          Box([fsTop,fsLeft],XPos,YPos,21,aWidthTituloBloco,'Hora de '+vEntSai,'',taCenter,True)
//       else
//          Box([fsTop,fsLeft],XPos,YPos,21,aHeigthPadrao,'Hora de '+vEntSai,TimeToStr(ide.hSaiEnt),taCenter,True);
		if infNFe.versao = 2.00 then
			vSaiEnt := ifthen(ide.hSaiEnt = 0, '', TimeToStr(ide.hSaiEnt))
		else
			vSaiEnt := ifthen(TimeOf(ide.dSaiEnt)=0, '', TimeToStr(ide.dSaiEnt));
		Box([fsTop,fsLeft],XPos,YPos,21,aHeigthPadrao,'Hora de '+vEntSai, vSaiEnt ,taCenter,True);
      end;
     Result:=YPos;
     TituloDoBloco([],PosY,PosX,YPos,'DESTINATÁRIO /','REMETENTE');
   end;
end;

function ImprimirLocalRetirada(PosX,
  PosY: Double): Double;
var vEnd:String;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     //Ocultar Local se não for informado CNPJ
     if EstaVazio(Retirada.CNPJCPF) then
      begin
        Result:=PosY;
        exit;
      end;

     PosX:=PosX+aWidthTituloBloco;
     with Retirada do
     begin
       if Length(Trim(CNPJCPF)) > 11 then
         Box([fstop],PosX,PosY,50,aHeigthPadrao+1,'CNPJ/CPF',FormatarCNPJ(CNPJCPF),taCenter)
       else
         Box([fstop],PosX,PosY,50,aHeigthPadrao+1,'CNPJ/CPF',FormatarCPF(CNPJCPF),taCenter);

       vEnd:=XLgr;
       if (Trim(Nro)>'') and (Nro<>'SN') then
          vEnd:=vEnd+' '+Nro;
       if Trim(XCpl)>'' then
          vEnd:=vEnd+', '+XCpl;
       vEnd:=vEnd+' - '+xBairro+' - '+xMun+' - '+UF;
       Box([fstop,fsLeft],XPos,YPos,21,aHeigthPadrao+1,'Endereço',vEnd,taLeftJustify,true);
     end;

     Result:=YPos;
     TituloDoBloco([fsTop],PosY,PosX,YPos,'RETI','RADA');
  end;
end;

function ImprimirLocalEntrega(PosX,
  PosY: Double): Double;
var vEnd:String;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     //Ocultar Local se não for informado CNPJ
     if EstaVazio(Entrega.CNPJCPF) then
      begin
        Result:=PosY;
        exit;
      end;

     PosX:=PosX+aWidthTituloBloco;
     with Entrega do
     begin
       if Length(Trim(CNPJCPF)) > 11 then
         Box([fstop],PosX,PosY,50,aHeigthPadrao+1,'CNPJ/CPF',FormatarCNPJ(CNPJCPF),taCenter)
       else
         Box([fstop],PosX,PosY,50,aHeigthPadrao+1,'CNPJ/CPF',FormatarCPF(CNPJCPF),taCenter);

       vEnd:=XLgr;
       if (Trim(Nro)>'') and (Nro<>'SN') then
          vEnd:=vEnd+' '+Nro;
       if Trim(XCpl)>'' then
          vEnd:=vEnd+', '+XCpl;
       vEnd:=vEnd+' - '+xBairro+' - '+xMun+' - '+UF;
       Box([fstop,fsLeft],XPos,YPos,21,aHeigthPadrao+1,'Endereço',vEnd,taLeftJustify,true);
     end;

     Result:=YPos;
     TituloDoBloco([fsTop],PosY,PosX,YPos,'ENTRE','GA');
  end;
end;

function ImprimirFaturas(PosX, PosY: Double): Double;
var i:Integer;
    aHeight, XX,YY,YY2: Double;
    q, f:integer;
    wtemp_FontSizeText: double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
      //Ocultar se não for informado nenhuma
      if (EstaVazio(Cobr.Fat.nFat)) then
      begin
         if (Cobr.Dup.Count=0) then
         begin
            //exibir somemte informação se não for OUTRAS
            if Ide.indPag=ipOutras then
            begin
              Result:=PosY;
              exit;
            end
            else
            begin
               //exibir somemte informação se não for OUTRAS
               PosX:=PosX+aWidthTituloBloco;
               if (ide.indPag=ipVista) then
                  Box([fstop],PosX,PosY,50,aHeigthPadrao,' ','PAGAMENTO À VISTA',taLeftJustify,True)
               else if (ide.indPag=ipPrazo) then
                  Box([fstop],PosX,PosY,50,aHeigthPadrao,' ','PAGAMENTO A PRAZO',taLeftJustify,True);
              Result:=YPos;
              TituloDoBloco([fsTop],PosY,PosX,YPos,'FATU','RAS');
              exit;
            end;
         end;
      end;

     PosX:=PosX+aWidthTituloBloco;
     YY2:=0;
     if not (EstaVazio(Cobr.Fat.nFat)) then
     begin
        Box([fstop,fsRigth],PosX,PosY,30,aHeigthPadrao,'Número da Fatura',Cobr.Fat.nFat,taLeftJustify);
        Box([fsLeft,fsRigth],XPos,YPos,30,aHeigthPadrao,'Valor Original',FormatFloat(Cobr.Fat.vOrig),taLeftJustify);
        Box([fsLeft,fsRigth],XPos,YPos,30,aHeigthPadrao,'Valor do Desconto',FormatFloat(Cobr.Fat.vDesc),taLeftJustify);
        Box([fsLeft],XPos,YPos,30,aHeigthPadrao,'Valor Líquido',FormatFloat(Cobr.Fat.vLiq),taLeftJustify,true);
        YY2:=aHeigthPadrao;
     end;
     YY:=PosY+YY2;
     XX:=PosX;
     if not (Cobr.Dup.Count=0) then
     begin
        wtemp_FontSizeText:=FontSizeText;
        if FontNameUsed = 'Courier New' then
           FontSizeText:=8
        else
           FontSizeText:=7;

        ClearAllTabs;
        for i:=1 to 4 do
         begin
           SetTab(XX+1,pjLeft,24,0,0,0);
           SetTab(XX+25,pjCenter,18,0,0,0);
           SetTab(XX+43,pjRight,19+IfThen(i=4,-1,0),0,0,0);
           XX:=XX+63;
         end;
        GotoXY(XX,YY);
        NewLine;
        SetFontTitle;
        Underline:=True;
        for i:=1 to 4 do
         begin
           PrintTab('NÚMERO');
           PrintTab('VENCIMENTO');
           PrintTab('VALOR');
         end;
        SetFontText;
        NewLine;
        q:=1;
        for f:=0 to Cobr.Dup.Count-1 do
         begin
          with Cobr.Dup.Items[f] do
           begin
             PrintTab(NDup);
             PrintTab(FormatDate(DateToStr(DVenc)));
             PrintTab(FormatFloat(VDup));
             Inc(q);
             if q>4 then
              begin
                NewLine;
                q:=1;
              end;
           end;
         end;

        //aHeight:=YPos-PosY-GetFontHeigh;
        aHeight:=YPos-PosY-YY2+1;

        Box([fsTop],PosX,YY,63,aHeight);
        Box([fsTop,fsLeft],XPos,YY,63,aHeight);
        Box([fsTop,fsLeft],XPos,YY,63,aHeight);
        Box([fsTop,fsLeft],XPos,YY,62,aHeight,'','',taLeftJustify,True);

        //if FontNameUsed = 'Courier New' then
           FontSizeText:=wtemp_FontSizeText;
     end
     else
      aHeight := 0;

     Result:=YPos;
     if ((aHeight+YY2) < 9) then
        TituloDoBloco([fsTop],PosY,PosX,YPos,'FATU','RAS')
     else if ((aHeight+YY2) < 11) then
        TituloDoBloco([fsTop],PosY,PosX,YPos,'FATURAS')
     else
        TituloDoBloco([fsTop],PosY,PosX,YPos,'FATURA /','DUPLICATAS');
   end;
end;

function ImprimirCalculoImposto(PosX, PosY: Double): Double;
var
  x: double;
  lVTotTrib: string;
  lTemp: string;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
  begin
    x:=0;
    if Total.ICMSTot.vTotTrib <> 0 then
      x := 11;

    PosX:=PosX+aWidthTituloBloco;
    Box([fsTop],PosX,PosY,50-x,aHeigthPadrao,'Base de Cálculo do ICMS',FormatFloat(Total.ICMSTot.VBC),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,50-x,aHeigthPadrao,'Valor do ICMS',FormatFloat(Total.ICMSTot.VICMS),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,50-x,aHeigthPadrao,'Base de Cálculo do ICMS Subst.',FormatFloat(Total.ICMSTot.vBCST),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,50-x,aHeigthPadrao,'Valor do ICMS Substituição',FormatFloat(Total.ICMSTot.vST),taRightJustify);
    if Total.ICMSTot.vTotTrib <> 0 then
    begin
      lVTotTrib :=FormatFloat(Total.ICMSTot.vTotTrib);
      if (TributosPercentual = ptValorProdutos) and (Total.ICMSTot.VProd > 0) then
        lVTotTrib :=lVTotTrib + '('+FormatFloat((Total.ICMSTot.vTotTrib*100)/( Total.ICMSTot.VProd - Total.ICMSTot.VDesc ))+'%)'
      else if (TributosPercentual = ptValorNF) and (Total.ICMSTot.VNF > 0) then
        lVTotTrib :=lVTotTrib + '('+FormatFloat((Total.ICMSTot.vTotTrib*100)/( Total.ICMSTot.VNF ))+'%)';
      lTemp:='V.Aprox.Tributos';
      if NaoEstaVazio(TributosFonte) then
        lTemp:=lTemp+' (Fonte:'+TributosFonte+')';
      Box([fsLeft],XPos,YPos,(x*4),aHeigthPadrao,lTemp,lVTotTrib,taRightJustify);
    end;
    Box([fsTop,fsLeft],XPos,YPos,51,aHeigthPadrao,'Valor Total dos Produtos',FormatFloat(Total.ICMSTot.VProd),taRightJustify,True);

    Box([fsTop],PosX,YPos,40,aHeigthPadrao,'Valor do Frete',FormatFloat(Total.ICMSTot.VFrete),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,40,aHeigthPadrao,'Valor do Seguro',FormatFloat(Total.ICMSTot.VSeg),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,39,aHeigthPadrao,'Desconto',FormatFloat(Total.ICMSTot.VDesc),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,40,aHeigthPadrao,'Outras Despesas Acessórias',FormatFloat(Total.ICMSTot.VOutro),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao,'Valor do IPI',FormatFloat(Total.ICMSTot.VIPI),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,51,aHeigthPadrao,'VALOR TOTAL DA NOTA FISCAL',FormatFloat(Total.ICMSTot.VNF),taRightJustify,True,false);

    Result:=YPos;
    TituloDoBloco([fsTop],PosY,PosX,YPos,'CALCULO','IMPOSTO');
    end;
end;

function ImprimirTransportadorVolumes(PosX,
  PosY: Double): Double;
const aIncHeigth:Double=0.5;
var i: integer;
    wfrete: string;
    wTemp_FontSizeText: double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     PosX:=PosX+aWidthTituloBloco;
     Box([fsTop],PosX,PosY,120,aHeigthPadrao+aIncHeigth,'Nome / Razão Social',Transp.Transporta.XNome);

     case Transp.ModFrete of
        mfContaEmitente:
               begin
                  wFrete:='0-EMITENTE';
               end;
        mfContaDestinatario:
               begin
                  wFrete:='1-DEST/REM';
               end;
        mfContaTerceiros:
               begin
                  wFrete:='2-TERCEIROS';
               end;
        mfSemFrete:
               begin
                  wFrete:='9-SEM FRETE';
               end;
     end;
     Box([fsTop,fsLeft],XPos,YPos,29,aHeigthPadrao+aIncHeigth,'Frete Por Conta',wfrete,taCenter);

     wTemp_FontSizeText:=FontSizeText;
     FontSizeText:=TamanhoFonte_ANTT;
     Box([fsTop,fsLeft],XPos,YPos,23,aHeigthPadrao+aIncHeigth,'Código ANTT',Transp.VeicTransp.RNTC,taCenter);
     FontSizeText:=wTemp_FontSizeText;

     Box([fsTop,fsLeft],XPos,YPos,27,aHeigthPadrao+aIncHeigth,'Placa do Veículo',Transp.VeicTransp.Placa,taCenter);
     Box([fsTop,fsLeft],XPos,YPos,14,aHeigthPadrao+aIncHeigth,'Estado',Transp.VeicTransp.UF,taCenter);
     if Length(Trim(Transp.Transporta.CNPJCPF)) > 11 then
       Box([fsTop,fsLeft],XPos,YPos,38,aHeigthPadrao+aIncHeigth,'CNPJ / CPF',FormatarCNPJ(Transp.Transporta.CNPJCPF),taCenter,True)
     else
       Box([fsTop,fsLeft],XPos,YPos,38,aHeigthPadrao+aIncHeigth,'CNPJ / CPF',FormatarCPF(Transp.Transporta.CNPJCPF),taCenter,True);
     Box([fsTop],PosX,YPos,132,aHeigthPadrao+aIncHeigth,'Endereço',Transp.Transporta.XEnder);
     Box([fsTop,fsLeft],XPos,YPos,65,aHeigthPadrao+aIncHeigth,'Município',Transp.Transporta.XMun,taCenter);
     Box([fsTop,fsLeft],XPos,YPos,14,aHeigthPadrao+aIncHeigth,'Estado',Transp.Transporta.UF,taCenter);
     Box([fsTop,fsLeft],XPos,YPos,40,aHeigthPadrao+aIncHeigth,'Inscrição Estadual',Transp.Transporta.IE,taCenter,True);

     if Transp.Vol.Count > 0 then
      begin
        for I := 0 to Transp.Vol.Count - 1 do
        begin
           //if (Transp.Vol.Items[i].qVol <> 0) then
              Box([fsTop],PosX,YPos,41,aHeigthPadrao+aIncHeigth,'Quantidade',IntToStr(Transp.Vol.Items[i].qVol),taRightJustify);
           //else
           //   Box([fsTop],PosX,YPos,41,aHeigthPadrao+aIncHeigth,'Quantidade','',taRightJustify);
           Box([fsTop,fsLeft],XPos,YPos,43,aHeigthPadrao+aIncHeigth,'Espécie',Transp.Vol.Items[i].esp,taCenter);
           Box([fsTop,fsLeft],XPos,YPos,44,aHeigthPadrao+aIncHeigth,'Marca',Transp.Vol.Items[i].marca,taCenter);
           Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao+aIncHeigth,'Numero',Transp.Vol.Items[i].nVol,taCenter);
           //if (Transp.Vol.Items[i].pesoB <> 0) then
              Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao+aIncHeigth,'Peso Bruto',FormatFloat(Transp.Vol.Items[i].pesoB,NotaUtil.PreparaCasasDecimais(3)),taRightJustify);
           //else
           //   Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao+aIncHeigth,'Peso Bruto','',taRightJustify);
           //if (Transp.Vol.Items[i].pesoL <> 0) then
              Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao+aIncHeigth,'Peso Líquido',FormatFloat(Transp.Vol.Items[i].pesoL,NotaUtil.PreparaCasasDecimais(3)),taRightJustify,True);
           //else
           //   Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao+aIncHeigth,'Peso Líquido','',taRightJustify,True);
        end;
      end
      else
      begin
        Box([fsTop],PosX,YPos,41,aHeigthPadrao+aIncHeigth,'Quantidade','',taRightJustify);
        Box([fsTop,fsLeft],XPos,YPos,43,aHeigthPadrao+aIncHeigth,'Espécie','',taCenter);
        Box([fsTop,fsLeft],XPos,YPos,44,aHeigthPadrao+aIncHeigth,'Marca','',taCenter);
        Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao+aIncHeigth,'Numero','',taCenter);
        Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao+aIncHeigth,'Peso Bruto','',taRightJustify);
        Box([fsTop,fsLeft],XPos,YPos,41,aHeigthPadrao+aIncHeigth,'Peso Líquido','',taRightJustify,True);
      end;

     Result:=YPos;
     TituloDoBloco([fsTop],PosY,PosX,YPos,'TRANSPORTADOR','VOLUMES TRANSP.');
   end;
end;

function ImprimirCalculoISSQN(PosX, PosY: Double): Double;
const aIncHeigth:Double=1;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     if Total.ISSQNtot.vServ>0 then
      begin
        PosX:=PosX+aWidthTituloBloco;
        Result:=PosY-aHeigthPadrao-aIncHeigth;
        Box([fsTop,fsBottom],PosX,Result,62,aHeigthPadrao+aIncHeigth,'Inscrição Municipal',Emit.IM);
        Box([fsTop,fsBottom,fsLeft],XPos,YPos,63,aHeigthPadrao+aIncHeigth,'Valor Total dos Serviços',FormatFloat(Total.ISSQNtot.vServ),taRightJustify);
        Box([fsTop,fsBottom,fsLeft],XPos,YPos,63,aHeigthPadrao+aIncHeigth,'Base de Cálculo do ISSQN',FormatFloat(Total.ISSQNtot.vBC),taRightJustify);
        Box([fsTop,fsBottom,fsLeft],XPos,YPos,63,aHeigthPadrao+aIncHeigth,'Valor do ISSQN',FormatFloat(Total.ISSQNtot.vISS),taRightJustify,True);
        TituloDoBloco([fsTop],Result,PosX,YPos,'CALC.','ISSQN');
        Result:=PosY-aHeigthPadrao-aIncHeigth;
      end
     else
        Result := PosY;
   end;
end;

function ImprimirDadosAdicionais(PosX,PosY,aHeigth: Double): Double;
var qLin : integer;
    wInfCpl, YFim:Double;

    TempPosX,TempPosY,TempHeight: Double;
    TempYFim, TempYPos, TempXPos, TempFLastX: Double;
    TempBaseReport: TBaseReport;
    TempMemoInfCpl: TMemoBuf;

begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
    //simulação de quantas linhas precisará para imprimir Dados Adicionais
    if ExpandirDadosAdicionaisAuto then
    begin
      TempPosY := PosY;
      TempYFim := YFim;
      TempYPos := YPos;
      TempPosX := PosX;
      TempFLastX := FLastX;
      TempXPos := XPos;
      PosX := PosX-800;
      FLastX := FLastX-800;
      XPos := XPos-800;
      TempHeight := aHeigth;
      TempMemoInfCpl := TMemoBuf.Create;
      try
        repeat
          if aHeigth <= 78 then
          begin
            TempHeight := Trunc(aHeigth);
            TempMemoInfCpl.Text := FMemoInfCpl.Text;

            YFim:=PosY;
            PosX:=PosX+aWidthTituloBloco;
            PosY:=PosY-aHeigth;
            wInfCpl:=176;
            if FPageNum>1 then
              wInfCpl:=FLastX-PosX;
            Box([],PosX,PosY,wInfCpl,aHeigth,'Informações Complementares');
            if FPageNum=1 then
              Box([fsLeft],XPos,YPos,75,aHeigth,'Reservado ao Fisco','',taLeftJustify,True);
            SetFont(FontNameUsed,TamanhoFonte_infComplementares); 
            //informacoes complementares
            GotoXY(PosX,PosY);
            NewLine;
            NewLine;
            TempMemoInfCpl.PrintStart:=PosX+1;
            TempMemoInfCpl.PrintEnd:=PosX+wInfCpl-2;
            TempMemoInfCpl.NoNewLine:=True;
            qLin := Trunc((YFim-YPos) / GetFontHeigh) - Trunc((aHeigth*2/29));
            TempMemoInfCpl.BaseReport:=BaseReport;
            TempMemoInfCpl.PrintLines(qLin,False);

            Result:=PosY;
            TituloDoBloco([fsTop],PosY,PosX,PosY+aHeigth,'DADOS ADICIONAIS');

            if not TempMemoInfCpl.Empty then
              aHeigth := Trunc((((qLin+4)*aHeigth)/qLin));
          end;
        until ((TempMemoInfCpl.Empty) or (aHeigth > 78));
      finally
        TempMemoInfCpl.Free;
        PosX := TempPosX;
        PosY := TempPosY;
        YFim := TempYFim;
        YPos := TempYPos;
        FLastX := TempFLastX;
        XPos := TempXPos;
      end;
      aHeigth := TempHeight;
    end;

     YFim:=PosY;
     PosX:=PosX+aWidthTituloBloco;
     PosY:=PosY-aHeigth;
     wInfCpl:=176;
     if FPageNum>1 then
        wInfCpl:=FLastX-PosX;
     Box([],PosX,PosY,wInfCpl,aHeigth,'Informações Complementares');
     if FPageNum=1 then
        Box([fsLeft],XPos,YPos,75,aHeigth,'Reservado ao Fisco','',taLeftJustify,True);
     SetFont(FontNameUsed,TamanhoFonte_infComplementares);
     //informacoes complementares
     GotoXY(PosX,PosY);
     NewLine;
     NewLine;
     FMemoInfCpl.PrintStart:=PosX+1;
     FMemoInfCpl.PrintEnd:=PosX+wInfCpl-2;
     FMemoInfCpl.NoNewLine:=True;
     qLin := Trunc((YFim-YPos) / GetFontHeigh) - Trunc((aHeigth*2/29));
     FMemoInfCpl.BaseReport:=BaseReport;
     FMemoInfCpl.PrintLines(qLin,False);

     Result:=PosY;
     TituloDoBloco([fsTop],PosY,PosX,PosY+aHeigth,'DADOS ADICIONAIS');
   end;
end;

function ImprimirRodape(PosX: Double): Double;
var vEnd:String;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     SetFontTitle;
     Result:=FLastY-GetFontHeigh;
     GotoXY(PosX,Result);
     NewLine;
     vEnd:='DATA E HORA DA IMPRESSÃO: '+FormatDateTime('dd/mm/yyyy hh:mm:ss',Now);
     if Trim(NomeDoUsuario)>'' then
        vEnd:=vEnd+' - '+NomeDoUsuario;
     PrintXY(PosX,YPos,vEnd);

     if Trim(NomeDoERP)>'' then
     begin
        vEnd:='Desenvolvido por '+NomeDoERP;
        PrintRight(vEnd,FLastX);
     end;
  end;
end;

procedure PrepararItens(PosX, FirstY, LastY: Double);
var aIncWidht,XX,YY:Double;
    i:Integer;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     if (SystemPrinter.MarginRight <> 5.1) then
        ColsWidth[2]:=ColsWidth[2]-(SystemPrinter.MarginRight-5.1);

     PosX:=PosX+aWidthTituloBloco;
     TituloDoBloco([],FirstY,PosX,LastY,'DADOS DOS PRODUTOS / SERVIÇOS');
     GotoXY(PosX,FirstY);
     YY:=YPos;
     XX:=XPos;
     Box([fsTop],XPos,YPos,FLastX-XPos,LastY-FirstY);
     ClearAllTabs;
     for i:=Low(ColsWidth) to High(ColsWidth) do
      begin
        if (i=High(ColsWidth)) then
           aIncWidht:=FLastX-(XX+0.5+ColsWidth[i])
        else
           aIncWidht:=0;
        SetTab(XX+0.5,pjCenter,ColsWidth[i]-1+aIncWidht,0,0,0);
        XX:=XX+ColsWidth[i];
        if (i<High(ColsWidth)) then
         begin
           MoveTo(XX,FirstY);
           LineTo(XX,LastY);
         end;
      end;
     GotoXY(PosX,YY);
     SetFont(FontNameUsed,FontSizeGroup);
     NewLine;
     for i:=Low(ColsTitle) to High(ColsTitle) do
     begin
         // #consult atech, o usuário resolve não imprimir o vr. desconto na linha do item
         if (i=4) and (emit.CRT = crtSimplesNacional)  then
            PrintTab('CSO')
         else if (i=10) and ImprimirDesconto then
         begin
             if (i=10) and ImprimirDescPorc then
                PrintTab('DESC. %')
             else
                PrintTab(ColsTitle[i]);
         end
         else if (i=11) and ImprimirTributosItem then
         begin
            PrintTab(ColsTitle[i]);
         end
         else
            PrintTab(ColsTitle[i]);
     end;
     NewLine;
     for i:=Low(ColsTitleAux) to High(ColsTitleAux) do
     begin
         // #consult atech, o usuário resolve não imprimir o vr. desconto na linha do item
         if (i=4) and (emit.CRT = crtSimplesNacional)  then
            PrintTab('SN')
         else if (i=9) and ImprimirValorLiquido then
            PrintTab('LÍQUIDO')
         else if (i=10) and ImprimirDesconto then
         begin
             if (i=10) and ImprimirDescPorc then
                PrintTab('')
             else
                PrintTab(ColsTitleAux[i]);
         end
         else if (i=11) and ImprimirTributosItem then
         begin
            PrintTab(ColsTitleAux[i]);
         end
         else
            PrintTab(ColsTitleAux[i]);
     end;
     NewLine;
     SetFont(FontNameUsed,FontSizeItens);
     MoveTo(PosX,YPos-(LineHeight/1.2));
     LineTo(FLastX,YPos-(LineHeight/1.2));
     ClearAllTabs;
     XX:=PosX;
     for i:=Low(ColsWidth) to High(ColsWidth) do
      begin
        if (i=High(ColsWidth)) then
           aIncWidht:=FLastX-(XX+0.5+ColsWidth[i])
        else
           aIncWidht:=0;
        SetTab(XX+0.5,ColsAlingment[i],ColsWidth[i]-1+aIncWidht,0,0,0);
        XX:=XX+ColsWidth[i];
      end;
   end;
end;

function MontarPagina:Double;
var aWidthNatOper,XX,YY:Double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
    SetPen(FColorBorders,psSolid,EspessuraBorda,pmCopy);
    Inc(FPageNum);
    Inc(FCurrentPage);
    if FPageNum=1 then
     begin
       FLastXX := FLastX;
       FNumeroNF:=FormatFloat('000000000',Ide.NNF);
       FNumeroNF:=Copy(FNumeroNF,1,3)+'.'+Copy(FNumeroNF,4,3)+'.'+Copy(FNumeroNF,7,3);
       //FEspelho:=Trim(procNFe.nProt)='';
       FEspelho:=false; //funcionalidade de espelho suspensa devido reclamações
       FChaveNFe:=RightStr(infNFe.ID,44);
     end
     else
       FLastX := FLastXX;

    XX:=MarginLeft;YY:=MarginTop;
    if PosicaoCanhoto = 0 then
      XX:=ImprimirCanhoto(XX,YY)
    else
    begin
      FLastX := ImprimirCanhoto(FLastX-16,YY);
    end;
    Result:=XX;

    ImprimirMensagensDeFundo(XX);
    XX:=ImprimirEmitente(XX,YY);
    XX:=ImprimirTituloDANFe(XX,YY);
    aWidthNatOper:=XX-Result;
    YY:=ImprimirCodigoBarras(XX,YY);
    YY:=ImprimirEmitenteOutrosDados(Result,YY,aWidthNatOper);

    SetPen(clBlack,psSolid,EspessuraBorda,pmCopy);
    //Imprime somente na primeira folha
    if FPageNum=1 then
     begin
       YY:=ImprimirRemetenteDestinatario(Result,YY);
       YY:=ImprimirLocalRetirada(Result,YY);
       YY:=ImprimirLocalEntrega(Result,YY);
       YY:=ImprimirFaturas(Result,YY);
       YY:=ImprimirCalculoImposto(Result,YY);
       YY:=ImprimirTransportadorVolumes(Result,YY);
       FLastItens:=ImprimirRodape(Result);
       FLastItens:=ImprimirDadosAdicionais(Result,FLastItens,29);
       FLastItens:=ImprimirCalculoISSQN(Result,FLastItens);
     end
    else begin
       FLastItens:=ImprimirRodape(Result);
       if (not IsPrintAllInfCpl) then
          if not IsPrintAllProd then
             FLastItens:=ImprimirDadosAdicionais(Result,FLastItens,29)
            else
             FLastItens:=ImprimirDadosAdicionais(Result,FLastItens,FLastItens-YY);
    end;

    if not IsPrintAllProd then
       PrepararItens(Result,YY,FLastItens);

    Result:=Result+aWidthTituloBloco;
   end;
end;

procedure ImprimirItens(PosX:Double);
var qPrinted, QtdeMin,j:Integer;
    aDescProduto, vEnd:String;
    Memo:TMemoBuf;
    aFontHeigth:Double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     aFontHeigth:=GetFontHeigh;
     qPrinted:=0;
     while (FDetIndex<Det.Count) and ((LinhasPorPagina=0) or (qPrinted<LinhasPorPagina)) do
      begin
        Inc(qPrinted);
        with Det.Items[FDetIndex] do
         begin
          aDescProduto:=Trim(Prod.XProd);

          QtdeMin:=0;
          if ImprimirDetalhamentoEspecifico then
          begin
             if Prod.veicProd.chassi<>'' then
              begin
                with Prod.veicProd do
                 begin
                  aDescProduto:=aDescProduto+#13+
                                '  CHASSI: '+Prod.veicProd.chassi+#13+
                                '  COMBUSTÍVEL: '+CombDescricao+#13+
                                '  COR: '+xCor+#13+
                                '  FAB./MOD.: '+IntToStr(anoFab)+'/'+IntToStr(anoMod)+#13+
//                                '  RENAVAM: '+RENAVAM+#13+
                                '  Nº DO MOTOR: '+nMotor;
                 end;
                QtdeMin:=QtdeMin+6;
              end;

             if Prod.med.Count > 0 then
              begin
                for j:=0 to Prod.med.Count-1 do
                 begin
                   with Prod.med.Items[j] do
                    begin
                     aDescProduto:=aDescProduto+
                                   ' - LOTE: '+nLote+
   //                                ' QTDADE: '+FormatFloat(qLote)+#13+
                                   ' FABR.: '+FormatDate(DateToStr(dFab))+
                                   ' VAL.: '+FormatDate(DateToStr(dVal))+
                                   IfThen(vPMC>0,' PMC: '+FormatFloat(vPMC),'');
                    end;
                   QtdeMin:=QtdeMin+1;
                 end;
              end;
          end;

          if Trim(infAdProd)>'' then
           begin
             aDescProduto:=aDescProduto+#13+StringReplace(infAdProd,';',#13,[rfReplaceAll]);
             Inc(QtdeMin);
             for j:=1 to Length(infAdProd) do
                if infAdProd[j]=';' then
                   Inc(QtdeMin);
           end;

          if Prod.cProd='5173' then
             QtdeMin:=QtdeMin;

          //Testa se a quantidade de linhas a ser impressa
          //ultrapassará o final do quadro dos itens,
          //e caso aconteça, cria uma nova página
          if (YPos+(aFontHeigth*QtdeMin))>FLastItens then
           begin
             Break;
           end
           else
            if FDetIndex>0 then
             begin
               MoveTo(PosX,YPos+0.1-aFontHeigth);
               LineTo(FLastX,YPos+0.1-aFontHeigth);
             end;

          PrintTab(Prod.CProd);
          PrintTab('');
          vEnd:=Prod.NCM;
          if Trim(Prod.EXTIPI)>'' then
             vEnd:=vEnd+'/'+Prod.EXTIPI;
          PrintTab(vEnd);

          case Emit.CRT of
	          crtSimplesNacional                         : PrintTab(OrigToStr(Imposto.ICMS.orig)+CSOSNIcmsToStr(Imposto.ICMS.CSOSN));
            crtRegimeNormal , crtSimplesExcessoReceita : PrintTab(OrigToStr(Imposto.ICMS.orig)+CSTICMSToStr(Imposto.ICMS.CST));
          end;

          PrintTab(Prod.CFOP);
          PrintTab(Prod.UCom);

          PrintTab(FormatFloat(Prod.QCom,IfThen(Mask_qCom='',NotaUtil.PreparaCasasDecimais(CasasDecimais_qCom),Mask_qCom)));
          PrintTab(FormatFloat(Prod.VUnCom,IfThen(Mask_vUnCom='',NotaUtil.PreparaCasasDecimais(CasasDecimais_vUnCom),Mask_vUnCom)));

          if ImprimirValorLiquido then
             PrintTab(FormatFloat(Prod.VProd-Prod.VDesc))
          else
             PrintTab(FormatFloat(Prod.VProd));

          // #consult atech, impressão do desconto?
          if ImprimirDesconto then
          begin
            if ImprimirDescPorc then
            begin
              if Prod.vDesc > 0 then
                 PrintTab(FormatFloat({RoundTo(}100-((((Prod.VUnCom*Prod.QCom)-Prod.vDesc)/(Prod.VUnCom*Prod.QCom))*100){,-1)})+'%')
              else
                 PrintTab(FormatFloat(Prod.vDesc));
            end
            else
              PrintTab(FormatFloat(Prod.vDesc));
          end
          else
             PrintTab('');

          if ImprimirTributosItem then
            PrintTab(FormatFloat(Imposto.vTotTrib))
          else
            PrintTab('');

          PrintTab(FormatFloat(Imposto.ICMS.vBC));
          PrintTab(FormatFloat(Imposto.ICMS.vBCST));
          PrintTab(FormatFloat(Imposto.ICMS.vICMSST));
          PrintTab(FormatFloat(Imposto.ICMS.vICMS));
          PrintTab(FormatFloat(Imposto.IPI.vIPI));
          PrintTab(FormatFloat(Imposto.ICMS.pICMS));
          PrintTab(FormatFloat(Imposto.IPI.pIPI));

          Memo:=TMemoBuf.Create;
          try
            Memo.PrintStart:=PosX+ColsWidth[1]+0.5;
            Memo.PrintEnd:=Memo.PrintStart+ColsWidth[2]-0.5;
            Memo.NoNewLine:=true;
            memo.text:=aDescProduto;
            PrintMemo(Memo,0,false);
          finally
            Memo.Free;
          end;

          // Beretta
          If Prod.UCom <> Prod.uTrib Then
             Begin
             NewLine;
             PrintTab('');  // 1
             PrintTab('');  // 2
             PrintTab('');  // 3
             PrintTab('');  // 4
             PrintTab('');  // 5
             PrintTab(Prod.uTrib);  // 6
             PrintTab(FormatFloat(Prod.qTrib,'0.0000'));
             PrintTab(FormatFloat(Prod.vUnTrib,'0.0000'));
             End ;
          inc(FDetIndex);
          NewLine;
         end;
      end;
   end;
end;

procedure ImprimirPaisagem(aRaveSystem:TDANFeRave);
var wtemp:Double;
    wInfcpl,wInfFisco:String;
    i:Integer;
    bInicio:boolean;
begin
  //tamanho padrao das colunas

  // #consult atech, ajustes no tamanho dos campos código e descrição principalmente
  // e inclusao da coluna valor dos tributos e possibilidade de ocultar a coluna desconto
  ColsWidth[1]:=15;
  ColsWidth[2]:=67;
  ColsWidth[3]:=13;
  ColsWidth[4]:=7;
  ColsWidth[5]:=7;
  ColsWidth[6]:=9;
  ColsWidth[7]:=15;
  ColsWidth[8]:=13;
  ColsWidth[9]:=13;
  ColsWidth[10]:=11;
  ColsWidth[11]:=11;
  ColsWidth[12]:=15;
  ColsWidth[13]:=15;
  ColsWidth[14]:=11;
  ColsWidth[15]:=11;
  ColsWidth[16]:=11;
  ColsWidth[17]:=7;
  ColsWidth[18]:=7;

  FontSizeText:=10;

  DANFeRave:=aRaveSystem;

  //ajusta tamanho da coluna codigo
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
  begin
    FontSizeText:=TamanhoFonte_DemaisCampos;

    if TamanhoCampoCodigo <> 0 then
    begin
      wtemp:=ColsWidth[1]-TamanhoCampoCodigo;
      ColsWidth[1]:=ColsWidth[1]-wtemp;
      ColsWidth[2]:=ColsWidth[2]+wtemp;
    end;

    // #consult atech
    if not ImprimirDesconto then
    begin
        ColsWidth[10]:=0;
        ColsWidth[ 1]:=ColsWidth[1]+2;
        ColsWidth[ 2]:=ColsWidth[2]+8;
    end;
    if not ImprimirTributosItem then
    begin
        //ColsWidth[ 1]:=ColsWidth[1]+2;
        ColsWidth[ 2]:=ColsWidth[2]+ColsWidth[11];
        ColsWidth[11]:=0;
    end;

    FDetIndex:=0;

    wInfFisco:='';
    for i:=0  to InfAdic.obsFisco.Count-1 do
    begin
      with InfAdic.obsFisco.Items[i] do
        wInfFisco:=wInfFisco+XCampo+': '+XTexto;
      wInfFisco:=wInfFisco+';';
    end;
    FMemoInfCpl.Text:=wInfFisco;
    if Trim(FMemoInfCpl.Text)>'' then
       FMemoInfCpl.Text:=FMemoInfCpl.Text+InfAdic.infAdFisco
    else
       FMemoInfCpl.Text:=InfAdic.infAdFisco;

    wInfcpl:='';
    for i:=0  to InfAdic.ObsCont.Count-1 do
    begin
      with InfAdic.ObsCont.Items[i] do
        wInfcpl:=wInfcpl+XCampo+': '+XTexto;
      wInfcpl:=wInfcpl+';';
    end;
    if Trim(FMemoInfCpl.Text)>'' then
      FMemoInfCpl.Text:=FMemoInfCpl.Text+';'+wInfCpl+InfAdic.infCpl
    else
      FMemoInfCpl.Text:=wInfCpl+InfAdic.infCpl;

     if Ide.tpEmis <> teNORMAL then
     begin
        case Ide.tpEmis of
           teFSDA,
           teContingencia,
           teSVCAN,
           teSVCRS,
           teSCAN:
                   begin
                     wInfCpl:='DANFE em Contingência - impresso em decorrência de problemas técnicos';
                   end;
        end;
        wInfCpl:=wInfCpl+';'+
        'DATA/HORA INÍCIO: '+IfThen(ide.dhCont = 0,' ',DateTimeToStr(ide.dhCont))+';'+
        'MOTIVO CONTINGÊNCIA: '+IfThen(EstaVazio(ide.xJust),' ',ide.xJust);

         FMemoInfCpl.Text:=FMemoInfCpl.Text+';;'+wInfCpl;
     end;

    FMemoInfCpl.Text:=StringReplace(FMemoInfCpl.Text,';',#13,[rfReplaceAll]);

    bInicio:=True;
    while not ((IsPrintAllProd) and (IsPrintAllInfCpl)) do begin
      if not bInicio then
         NewPage;
      ImprimirItens(MontarPagina);
      bInicio:=False;
    end;

  end;
end;

end.
