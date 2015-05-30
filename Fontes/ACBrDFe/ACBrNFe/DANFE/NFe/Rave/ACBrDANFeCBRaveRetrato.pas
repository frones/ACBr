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
unit ACBrDANFeCBRaveRetrato;

interface

uses Graphics, Forms, Windows, SysUtils, Classes,
     Variants, DBClient, Math, StdCtrls, DB, Dialogs,
     Controls, ExtCtrls, Mask, MaskUtils, DateUtils,
     {$IFNDEF COMPILER16} JPEG, {$ELSE} Vcl.Imaging.jpeg, {$ENDIF}
     RpDefine, RpBase, RpSystem, RpBars, RpMemo,
     RpRenderText, RpRenderRTF, RpRenderHTML, RpRender, RpRenderPDF,
     ACBrNFe, pcnConversao, ACBrDANFeCBRave;

const
      FontSizeGroup:Integer=7;
      FontSizeTitle:Integer=6;
//      FontSizeText:Integer=8;

var
   FontSizeText:double;
   ColsWidth:array[1..18] of Double;  // #consult atech

procedure ImprimirRetrato(aRaveSystem:TDANFeRave);

implementation

uses ACBrNFeUtil, ACBrDFeUtil, StrUtils, pcnNFe;

function ImprimirCanhoto(PosX,PosY:Double):Double;
var aWidthOutros,aWidthNFe,
    aWidthData,
    aHeigth,aHeigthReceb: Double;
    vEnd: string;
    wtemp_FontSizeText : Double;  // #consult atech
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     aWidthNFe:=35;
     aHeigth:=18;
     aHeigthReceb:=10;
     aWidthOutros:=FLastX-FFirstX-aWidthNFe;
     aWidthData:=40;
     if FPageNum=1 then
      begin
        if (not FormularioContinuo) then
        begin
          Box([],FLastX-aWidthNFe,PosY,aWidthNFe,aHeigth);
          Box([fsRigth],PosX,PosY,aWidthOutros,aHeigthReceb,'','',taLeftJustify,True,False,False);
          Box([fsTop],PosX,YPos,aWidthData,aHeigth-aHeigthReceb,'DATA DE RECEBIMENTO','');
          Box([fsTop,fsRigth],XPos,YPos,aWidthOutros-aWidthData,aHeigth-aHeigthReceb,'IDENTIFICAÇÃO E ASSINATURA DO RECEBEDOR','');

          // #consult atech, para ajustar a impressão do canhoto pois dependendo dos dados
          // estrapola tudo
          wtemp_FontSizeText:=FontSizeText;
          if FontNameUsed = 'Courier New' then
             FontSizeText:=6.9
          else
             FontSizeText:=8;
          SetFont(FontNameUsed,FontSizeText);

          Bold:=True;
          GotoXY(0,PosY+GetFontHeigh);
          //NewLine;
          vEnd:='Recebemos de '+Emit.XNome+' os produtos/serviços constantes da NFe indicada ao lado';
          if FontNameUsed = 'Courier New' then
          begin
             if Length(vEnd)>108 then
             begin
                vEnd:='Recebemos de '+Emit.XNome;
                PrintCenter(vEnd,PosX+(aWidthOutros/2));
                NewLine;
                vEnd:='os produtos/serviços constantes da NFe indicada ao lado';
                PrintCenter(vEnd,PosX+(aWidthOutros/2));
             end
             else
             begin
                PrintCenter(vEnd,PosX+(aWidthOutros/2));
             end;
          end
          else
          begin
			       if Length(vEnd)>100 then
             begin
                vEnd:='Recebemos de '+Emit.XNome;
                PrintCenter(vEnd,PosX+(aWidthOutros/2));
                NewLine;
                vEnd:='os produtos/serviços constantes da NFe indicada ao lado';
                PrintCenter(vEnd,PosX+(aWidthOutros/2));
             end
             else
             begin
                PrintCenter(vEnd,PosX+(aWidthOutros/2));
             end;
          end;
          if ExibirResumoCanhoto then
          begin
             NewLine;
             if EstaVazio(ExibirResumoCanhoto_Texto) then
                PrintCenter('Emissão:'+FormatDate(DateToStr(Ide.DEmi))+' Dest/Rem:'+Dest.XNome+' Total:'+FormatFloat(Total.ICMSTot.VNF),PosX+(aWidthOutros/2))
             else
                PrintCenter(ExibirResumoCanhoto_Texto,PosX+(aWidthOutros/2));
          end;
        end;

        // #consult atech, retorno as configurações do canhoto
        FontSizeText:=wtemp_FontSizeText;
        SetFont(FontNameUsed,FontSizeText);

        Bold:=True;
        GotoXY(0,PosY+GetFontHeigh);
        NewLine;
        PrintCenter('NF-e',FLastX-aWidthNFe+(aWidthNFe/2));
        NewLine;
        PrintCenter('Nº: '+FNumeroNF,FLastX-aWidthNFe+(aWidthNFe/2));
        NewLine;
        PrintCenter('SÉRIE: '+FSerie,FLastX-aWidthNFe+(aWidthNFe/2));

        if (not FormularioContinuo) then
        begin
          SetPen(FColorBorders,psDot,EspessuraBorda,pmCopy);
          if PosicaoCanhoto = 0 then
          begin
            MoveTo(PosX,PosY+aHeigth+3);
            LineTo(FLastX,PosY+aHeigth+3);
          end
          else
          begin
            MoveTo(PosX,PosY-3);
            LineTo(FLastX,PosY-3);
          end;

          SetPen(FColorBorders,psSolid,EspessuraBorda,pmCopy);
        end;
      end;

     if PosicaoCanhoto = 0 then
       Result:=PosY+aHeigth+6
     else
      Result:=PosY-6;
  end;
end;

procedure ImprimirMensagensDeFundo;
var CenterX,YY:Double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     YY:=FLastY-5;
     if Ide.TpAmb=taHomologacao then
      begin //homologação
        SetFont(FontNameUsed,25);
        FontColor:=clSilver;
        Bold:=True;
        Underline:=True;
        GotoXY(FFirstX+5,YY);
        FontRotation:=45;
        Print('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
      end
     else if ((procNFe.cStat in [101,151]) or (NFeCancelada)) then
      begin //NFe Cancelada
        SetFont(FontNameUsed,25);
        FontColor:=clRed;
        Bold:=True;
        Underline:=True;
        GotoXY(FFirstX+80,YY-80);
        FontRotation:=45;
        Print('NFe Cancelada');
      end
     else if (procNFe.cStat = 110) then
      begin //NFe denegada
        SetFont(FontNameUsed,25);
        FontColor:=clRed;
        Bold:=True;
        Underline:=True;
        GotoXY(FFirstX+80,YY-80);
        FontRotation:=45;
        Print('NFe Denegada');
      end
     else if ((procNFe.cStat <> 100 ) and
              (Ide.tpEmis <> teFSDA) and
              (Ide.tpEmis <> teCONTINGENCIA) and
              (Length(Trim(ProtocoloNFe)) < 15)) then
      begin //Não autorizada
        SetFont(FontNameUsed,25);
        FontColor:=clRed;
        Bold:=True;
        Underline:=True;
        GotoXY(FFirstX+5,YY);
        FontRotation:=45;
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
        //GotoXY(FFirstX+15,YY-10);
        FontRotation:=45;
        CenterX:=XPos+((PageWidth-MarginRight-XPos)/2);
        PrintCenter(MarcaDaguaMSG,CenterX);
      end;

     SetFont(FontNameUsed,22);
     FontColor:=clSilver;
     Bold:=True;
     GotoXY(FFirstX+5,YY-10);
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
var aHeigthLogo, aWidthLogo, aWidth, CenterX:Double;
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
       aWidth:=85;
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
         if length(vEnd)>30 then
         begin
           vtemp := DivideTexto(vEnd,30);
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
         aWidthTexto:=48;
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
           aWidthTexto:=38;
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
     if FontNameUsed = 'Courier New' then
        aWidth:=37
     else
        aWidth:=36;
     Result:=PosX+aWidth;
     Box([fsLeft],PosX,PosY,aWidth,30);
     CenterX:=PosX+(aWidth/2);
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

     PrintCenter('SÉRIE '+IntToStr(Ide.Serie)+'-FOLHA '+PIVar(VarNumPage),CenterX);
     Bold:=False;
  end;
end;

function ImprimirCodigoBarras(PosX, PosY: Double):Double;
var PosYCodBarraContigencia, aWidth, CenterX:Double;
    w1,w2,w3,aTituloChave, aChaveAcesso, aProtocolo, aChaveContigencia:String;
    wTemp_FontSizeText: double;
begin
   with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
      aWidth:=FLastX-PosX;
      Box([fsLeft],PosX,PosY,aWidth,12.27,'','',taLeftJustify,True);
      if FEspelho then
         aChaveAcesso:='SEM VALOR FISCAL (SOMENTE CONFERENCIA)'
      else
         aChaveAcesso:=NotaUtil.FormatarChaveAcesso(FChaveNFe);
      if FormularioContinuo then
         aTituloChave:=' '
        else
         aTituloChave:='CHAVE DE ACESSO';
      wtemp_FontSizeText:=FontSizeText;
      if FontNameUsed = 'Courier New' then
         FontSizeText:=6.3
      else
         FontSizeText:=8;
      Box([fsLeft,fsTop],PosX,YPos,aWidth,aHeigthPadrao,aTituloChave,aChaveAcesso,taCenter,True);
      FontSizeText:=wtemp_FontSizeText;

      if ACBrNFe.NotasFiscais.Items[FNFIndex].NFe.Ide.tpEmis in [teContingencia,teFSDA] then
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
            BarWidth:=0.254;
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
               BarWidth:=0.254;
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
            SetFont(FontNameUsed,IfThen(Pos('Courier',FontNameUsed)>0,8,9));
            if OnlyNumber(aProtocolo)='' then
            begin
              Bold:=True;
              FontColor:=clRed;
              w1:='N F E   A I N D A   N Ã O   F O I';
              w2:='A U T O R I Z A D A   P E L A';
              w3:=' S E F A Z  (SEM VALIDADE FISCAL)';
            end
            else
            begin
              w1:='Consulta de autenticidade no portal nacional';
              w2:='da NF-e www.nfe.fazenda.gov.br/portal ou';
              w3:='no site da Sefaz Autorizadora';
            end;
            GotoXY(PosX,PosYCodBarraContigencia+GetFontHeigh+0.5);
            CenterX:=PosX+(aWidth/2);
            PrintCenter(w1,CenterX);
            NewLine;
            PrintCenter(w2,CenterX);
            NewLine;
            PrintCenter(w3,CenterX);
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
     if (not FormularioContinuo) then
        wTemp:='NATUREZA DA OPERAÇÃO'
       else
        wTemp:=' ';
     Box([fsTop],PosX,PosY,WidthNaturezaOperacao,aHeigthPadrao,wTemp,ide.natOp,taLeftJustify,True,False,False);
     if (not FormularioContinuo) then
     begin
       Box([fsTop],PosX,YPos,82,aHeigthPadrao,'INSCRIÇÃO ESTADUAL',Emit.IE,taLeftJustify);
       Box([fsTop,fsLeft],XPos,YPos,75,aHeigthPadrao,'INSCRIÇÃO ESTADUAL DO SUBST. TRIBUTÁRIO',Emit.IEST,taLeftJustify);
       Box([fsTop,fsLeft],XPos,YPos,87,aHeigthPadrao,'C.N.P.J.',FormatarCNPJ(Emit.CNPJCPF),taLeftJustify,True);
     end
     else begin
       Box([fsTop,fsLeft],XPos,YPos,87,aHeigthPadrao,' ','',taLeftJustify,True);
     end;
     Result:=YPos;
  end;
end;

procedure TituloDoBloco(PosX,PosY:Double;aDescricao: string);
begin
  with DANFeRave, DANFeRave.BaseReport do begin
    SetFont(FontNameUsed,FontSizeGroup);
    Bold:=True;
    PrintXY(PosX,PosY+3,aDescricao);
    Bold:=False;
    GotoXY(PosX,YPos+0.5);
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

     TituloDoBloco(PosX,PosY,'DESTINATÁRIO / REMETENTE');
     Box([],PosX,YPos,132,aHeigthPadrao,'Nome / Razão Social',Dest.XNome);
     if Length(Trim(Dest.CNPJCPF)) > 11 then
       Box([fsLeft],XPos,YPos,41,aHeigthPadrao,'CNPJ / CPF',FormatarCNPJ(Dest.CNPJCPF),taCenter)
     else
       Box([fsLeft],XPos,YPos,41,aHeigthPadrao,'CNPJ / CPF',FormatarCPF(Dest.CNPJCPF),taCenter);
     Box([fsLeft],XPos,YPos,21,aHeigthPadrao,'Data de Emissão',FormatDate(DateToStr(Ide.DEmi)),taCenter,True);
     with Dest.EnderDest do
      begin
       vEnd:=XLgr;
       if (Trim(Nro)>'') and (Nro<>'SN') then
          vEnd:=vEnd+' '+Nro;
       if Trim(XCpl)>'' then
          vEnd:=vEnd+', '+XCpl;
       Box([fsTop],PosX,YPos,93,aHeigthPadrao,'Endereço',vEnd);
       Box([fsTop,fsLeft],XPos,YPos,50,aHeigthPadrao,'Bairro',XBairro);
       Box([fsTop,fsLeft],XPos,YPos,30,aHeigthPadrao,'CEP',NotaUtil.FormatarCEP(Poem_Zeros(CEP,8)),taCenter);
       Box([fsTop,fsLeft],XPos,YPos,21,aHeigthPadrao,'Data de '+vEntSai,FormatDate(DateOf(ide.dSaiEnt)),taCenter,True);
       Box([fsTop],PosX,YPos,85,aHeigthPadrao,'Município',XMun);
       Box([fsTop,fsLeft],XPos,YPos,40,aHeigthPadrao,'Fone / Fax',NotaUtil.FormatarFone(Fone),taCenter);
       Box([fsTop,fsLeft],XPos,YPos,10,aHeigthPadrao,'Estado',UF,taCenter);
       Box([fsTop,fsLeft],XPos,YPos,38,aHeigthPadrao,'Inscrição Estadual',Dest.IE,taCenter);

//       if ide.hSaiEnt = 0 then
//          Box([fsTop,fsLeft],XPos,YPos,21,aHeigthPadrao,'Hora de '+vEntSai,'',taCenter,True)
//       else
//          Box([fsTop,fsLeft],XPos,YPos,21,aHeigthPadrao,'Hora de '+vEntSai,TimeToStr(ide.hSaiEnt),taCenter,True);
		if infNFe.versao = 2.00 then
			vSaiEnt := ifthen(ide.hSaiEnt = 0, '', TimeToStr(ide.hSaiEnt))
		else
			vSaiEnt := ifthen(TimeOf(ide.dSaiEnt)=0, '', TimeToStr(ide.dSaiEnt));
		Box([fsTop,fsLeft],XPos,YPos,21,aHeigthPadrao,'Hora de '+vEntSai, vSaiEnt ,taCenter,True);
     end;
     Result:=YPos;
  end;
end;

function ImprimirLocalRetirada(PosX,
  PosY: Double): Double;
var vEnd:String;
    wtemp_FontSizeText: double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     //Ocultar Local se não for informado CNPJ
     if EstaVazio(Retirada.CNPJCPF) then
      begin
        Result:=PosY;
        exit;
      end;

     TituloDoBloco(PosX,PosY,'LOCAL DE RETIRADA');
     with Retirada do
     begin
       wtemp_FontSizeText:=FontSizeText;
       if FontNameUsed = 'Courier New' then
          FontSizeText:=8;
       if Length(Trim(CNPJCPF)) > 11 then
         Box([],XPos,YPos,34,aHeigthPadrao,'CNPJ/CPF',FormatarCNPJ(CNPJCPF),taCenter)
       else
         Box([],XPos,YPos,34,aHeigthPadrao,'CNPJ/CPF',FormatarCPF(CNPJCPF),taCenter);
       FontSizeText:=wtemp_FontSizeText;

       vEnd:=XLgr;
       if (Trim(Nro)>'') and (Nro<>'SN') then
          vEnd:=vEnd+' '+Nro;
       if Trim(XCpl)>'' then
          vEnd:=vEnd+', '+XCpl;
       vEnd:=vEnd+' - '+xBairro+' - '+xMun+' - '+UF;
       Box([fsLeft],XPos,YPos,21,aHeigthPadrao,'Endereço',vEnd,taLeftJustify,true);
     end;

     Result:=YPos;
  end;
end;

function ImprimirLocalEntrega(PosX,
  PosY: Double): Double;
var vEnd:String;
    wtemp_FontSizeText: double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     //Ocultar Local se não for informado CNPJ
     if EstaVazio(Entrega.CNPJCPF) then
      begin
        Result:=PosY;
        exit;
      end;

     TituloDoBloco(PosX,PosY,'LOCAL DE ENTREGA');
     with Entrega do
     begin
       wtemp_FontSizeText:=FontSizeText;
       if FontNameUsed = 'Courier New' then
          FontSizeText:=8;
       if Length(Trim(CNPJCPF)) > 11 then
         Box([],XPos,YPos,34,aHeigthPadrao,'CNPJ/CPF',FormatarCNPJ(CNPJCPF),taCenter)
       else
         Box([],XPos,YPos,34,aHeigthPadrao,'CNPJ/CPF',FormatarCPF(CNPJCPF),taCenter);
       FontSizeText:=wtemp_FontSizeText;

       vEnd:=XLgr;
       if (Trim(Nro)>'') and (Nro<>'SN') then
          vEnd:=vEnd+' '+Nro;
       if Trim(XCpl)>'' then
          vEnd:=vEnd+', '+XCpl;
       vEnd:=vEnd+' - '+xBairro+' - '+xMun+' - '+UF;
       Box([fsLeft],XPos,YPos,21,aHeigthPadrao,'Endereço',vEnd,taLeftJustify,True);
     end;

     Result:=YPos;
  end;
end;

function ImprimirFaturas(PosX, PosY: Double): Double;
var i:Integer;
    aHeight, XX,YY,YY2:Double;
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
               TituloDoBloco(PosX,PosY,'FATURA/DUPLICATAS');
               if Ide.indPag=ipVista then
                  Box([],XPos,YPos,35,aHeigthPadrao,' ','PAGAMENTO À VISTA',taLeftJustify,True)
               else if Ide.indPag=ipPrazo then
                  Box([],XPos,YPos,35,aHeigthPadrao,' ','PAGAMENTO A PRAZO',taLeftJustify,True);
               Result:=PosY+aHeigthPadrao+LineHeight;
               exit;
            end;
         end;
      end;

     TituloDoBloco(PosX,PosY,'FATURA/DUPLICATAS');
     YY2:=0;
     if not (EstaVazio(Cobr.Fat.nFat)) then
     begin
        Box([fsRigth],XPos,YPos,30,aHeigthPadrao,'Número da Fatura',Cobr.Fat.nFat,taLeftJustify);
        Box([fsLeft,fsRigth],XPos,YPos,30,aHeigthPadrao,'Valor Original',FormatFloat(Cobr.Fat.vOrig),taLeftJustify);
        Box([fsLeft,fsRigth],XPos,YPos,30,aHeigthPadrao,'Valor do Desconto',FormatFloat(Cobr.Fat.vDesc),taLeftJustify);
        Box([fsLeft],XPos,YPos,30,aHeigthPadrao,'Valor Líquido',FormatFloat(Cobr.Fat.vLiq),taLeftJustify,true);
        YY2:=aHeigthPadrao;
     end;
     YY:=YPos;
     XX:=PosX;
     if not (Cobr.Dup.Count=0) then
     begin
        wtemp_FontSizeText:=FontSizeText;
        if FontNameUsed = 'Courier New' then
           FontSizeText:=8
        else
           FontSizeText:=7;

        ClearAllTabs;
        for i:=1 to 3 do
         begin
           SetTab(XX+1,pjLeft,25,0,0,0);
           SetTab(XX+22,pjCenter,19,0,0,0);
           SetTab(XX+42,pjRight,20,0,0,0);
           XX:=XX+67;
         end;
        GotoXY(XX,YY);
        NewLine;
        SetFontTitle;
        Underline:=True;
        for i:=1 to 3 do
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
            if q>3 then
             begin
               NewLine;
               q:=1;
             end;
           end;
         end;

        aHeight:=YPos-PosY-GetFontHeigh-YY2 + 1;

        Box([],PosX,YY,67,aHeight);
        Box([fsLeft],XPos,YY,67,aHeight);
        Box([fsLeft],XPos,YY,67,aHeight,'','',taLeftJustify,True);

        //if FontNameUsed = 'Courier New' then
           FontSizeText:=wtemp_FontSizeText;
     end;
     Result:=YPos;
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
      x := 10;

    TituloDoBloco(PosX,PosY,'CÁLCULO DO IMPOSTO');
    Box([],PosX,YPos,38.5-x,aHeigthPadrao,'Base de Cálc. ICMS',FormatFloat(Total.ICMSTot.VBC),taRightJustify);
    Box([fsLeft],XPos,YPos,38.5-x,aHeigthPadrao,'Valor do ICMS',FormatFloat(Total.ICMSTot.VICMS),taRightJustify);
    Box([fsLeft],XPos,YPos,38.5-x,aHeigthPadrao,'Base Cálc. ICMS Subst.',FormatFloat(Total.ICMSTot.vBCST),taRightJustify);
    Box([fsLeft],XPos,YPos,38.5-x,aHeigthPadrao,'Valor ICMS Subst.',FormatFloat(Total.ICMSTot.vST),taRightJustify);
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
    Box([fsLeft],XPos,YPos,40,aHeigthPadrao,'Valor Total dos Produtos',FormatFloat(Total.ICMSTot.VProd),taRightJustify,True);

    Box([fsTop],PosX,YPos,30.8,aHeigthPadrao,'Valor do Frete',FormatFloat(Total.ICMSTot.VFrete),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,30.8,aHeigthPadrao,'Valor do Seguro',FormatFloat(Total.ICMSTot.VSeg),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,30.8,aHeigthPadrao,'Desconto',FormatFloat(Total.ICMSTot.VDesc),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,30.8,aHeigthPadrao,'Outras Desp. Acessórias',FormatFloat(Total.ICMSTot.VOutro),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,30.8,aHeigthPadrao,'Valor do IPI',FormatFloat(Total.ICMSTot.VIPI),taRightJustify);
    Box([fsTop,fsLeft],XPos,YPos,40,aHeigthPadrao,'VALOR TOTAL DA NOTA FISCAL',FormatFloat(Total.ICMSTot.VNF),taRightJustify,True,False);

    Result:=YPos;
  end;
end;

function ImprimirTransportadorVolumes(PosX,
  PosY: Double): Double;
var
   wTemp_FontSizeText: double;
   i: integer;
   wFrete: string;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     TituloDoBloco(PosX,PosY,'TRANSPORTADOR / VOLUMES TRANSPORTADOS');

     Box([],PosX,YPos,90,aHeigthPadrao,'Nome / Razão Social',Transp.Transporta.XNome);

     wTemp_FontSizeText:=FontSizeText;
     FontSizeText:=8;
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
     Box([fsLeft],XPos,YPos,27,aHeigthPadrao,'Frete Por Conta',wFRETE,taCenter);
     FontSizeText:=wTemp_FontSizeText;

     wTemp_FontSizeText:=FontSizeText;
     FontSizeText:=TamanhoFonte_ANTT;
     Box([fsLeft],XPos,YPos,19,aHeigthPadrao,'Código ANTT',Transp.VeicTransp.RNTC,taCenter);
     FontSizeText:=wTemp_FontSizeText;

     Box([fsLeft],XPos,YPos,22,aHeigthPadrao,'Placa do Veículo',Transp.VeicTransp.Placa,taCenter);
     Box([fsLeft],XPos,YPos,8,aHeigthPadrao,'UF',Transp.VeicTransp.UF,taCenter);

     wtemp_FontSizeText:=FontSizeText;
     if FontNameUsed = 'Courier New' then
        FontSizeText:=8;
     if Length(TRim(Transp.Transporta.CNPJCPF)) > 11 then
       Box([fsLeft],XPos,YPos,30,aHeigthPadrao,'CNPJ / CPF',FormatarCNPJ(Transp.Transporta.CNPJCPF),taCenter,True)
     else
       Box([fsLeft],XPos,YPos,30,aHeigthPadrao,'CNPJ / CPF',FormatarCPF(Transp.Transporta.CNPJCPF),taCenter,True);
     FontSizeText:=wtemp_FontSizeText;

     Box([fsTop],PosX,YPos,90,aHeigthPadrao,'Endereço',Transp.Transporta.XEnder);
     Box([fsTop,fsLeft],XPos,YPos,68,aHeigthPadrao,'Município',Transp.Transporta.XMun,taCenter);
     Box([fsTop,fsLeft],XPos,YPos,8,aHeigthPadrao,'UF',Transp.Transporta.UF,taCenter);
     Box([fsTop,fsLeft],XPos,YPos,30,aHeigthPadrao,'Inscrição Estadual',Transp.Transporta.IE,taCenter,True);

     if Transp.Vol.Count > 0 then
      begin
        for I := 0 to Transp.Vol.Count - 1 do
        begin
           //if (Transp.Vol.Items[i].qVol <> 0) then
              Box([fsTop],PosX,YPos,20,aHeigthPadrao,'Quantidade',IntToStr(Transp.Vol.Items[i].qVol),taRightJustify);
           //else
           //   Box([fsTop],PosX,YPos,20,aHeigthPadrao,'Quantidade','',taRightJustify);
           Box([fsTop,fsLeft],XPos,YPos,34,aHeigthPadrao,'Espécie',Transp.Vol.Items[i].esp,taCenter);
           Box([fsTop,fsLeft],XPos,YPos,50,aHeigthPadrao,'Marca',Transp.Vol.Items[i].marca,taCenter);
           Box([fsTop,fsLeft],XPos,YPos,30,aHeigthPadrao,'Numero',Transp.Vol.Items[i].nVol,taCenter);
           //if (Transp.Vol.Items[i].pesoB <> 0) then
              Box([fsTop,fsLeft],XPos,YPos,30,aHeigthPadrao,'Peso Bruto',FormatFloat(Transp.Vol.Items[i].pesoB,NotaUtil.PreparaCasasDecimais(3)),taRightJustify);
           //else
           //   Box([fsTop,fsLeft],XPos,YPos,30,aHeigthPadrao,'Peso Bruto','',taRightJustify);
           //if (Transp.Vol.Items[i].pesoL <> 0) then
              Box([fsTop,fsLeft],XPos,YPos,30,aHeigthPadrao,'Peso Líquido',FormatFloat(Transp.Vol.Items[i].pesoL,NotaUtil.PreparaCasasDecimais(3)),taRightJustify,True);
           //else
           //   Box([fsTop,fsLeft],XPos,YPos,30,aHeigthPadrao,'Peso Líquido','',taRightJustify,True);
        end;
     end;
     
     Result:=YPos;
  end;
end;

function ImprimirCalculoISSQN(PosX, PosY: Double): Double;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     if Total.ISSQNtot.vServ>0 then
      begin
        Result:=PosY-aHeigthPadrao-4;
        TituloDoBloco(PosX,Result,'CÁLCULO DO ISSQN');
        Box([],PosX,YPos,50,aHeigthPadrao,'Inscrição Municipal',Emit.IM);
        Box([fsLeft],XPos,YPos,48,aHeigthPadrao,'Valor Total dos Serviços',FormatFloat(Total.ISSQNtot.vServ),taRightJustify);
        Box([fsLeft],XPos,YPos,48,aHeigthPadrao,'Base de Cálculo do ISSQN',FormatFloat(Total.ISSQNtot.vBC),taRightJustify);
        Box([fsLeft],XPos,YPos,48,aHeigthPadrao,'Valor do ISSQN',FormatFloat(Total.ISSQNtot.vISS),taRightJustify,True);
      end
     else
        Result:=PosY;
  end;
end;

function ImprimirDadosAdicionais(PosX,PosY,aHeigth: Double): Double;
var wInfCpl, YFim:Double;
    qLin : integer;
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
          if aHeigth <= 110 then
          begin
            TempHeight := Trunc(aHeigth);
            TempMemoInfCpl.Text := FMemoInfCpl.Text;
            PosY:=PosY-aHeigth-4;
            TituloDoBloco(PosX,PosY,'DADOS ADICIONAIS');
            YFim:=YPos+aHeigth;

            wInfCpl:=124;
            if FPageNum>1 then
              wInfCpl:=FLastX-XPos;
            Box([],PosX,YPos,wInfCpl,aHeigth,'Informações Complementares');
            if FPageNum=1 then
              Box([fsLeft],XPos,YPos,70,aHeigth,'Reservado ao Fisco','',taLeftJustify,True);

            SetFont(FontNameUsed,TamanhoFonte_infComplementares); 
            //informacoes complementares
            GotoXY(PosX,PosY+4);
            NewLine;
            NewLine;

            TempMemoInfCpl.BaseReport := BaseReport;
            TempMemoInfCpl.PrintStart := PosX + 1;
            TempMemoInfCpl.PrintEnd   := PosX + wInfCpl - 2;
            TempMemoInfCpl.NoNewLine  := True;
            qLin := Trunc((YFim-YPos) / GetFontHeigh) - Trunc((aHeigth*2/32));
            TempMemoInfCpl.PrintLines(qLin,False);
            if not TempMemoInfCpl.Empty then
              aHeigth := Trunc((((qLin+4)*aHeigth)/qLin));
          end;
        until ((TempMemoInfCpl.Empty) or (aHeigth > 110));
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

     PosY:=PosY-aHeigth-4;
     TituloDoBloco(PosX,PosY,'DADOS ADICIONAIS');
     YFim:=YPos+aHeigth;

     wInfCpl:=124;
     if FPageNum>1 then
        wInfCpl:=FLastX-XPos;
     Box([],PosX,YPos,wInfCpl,aHeigth,'Informações Complementares');
     if FPageNum=1 then
        Box([fsLeft],XPos,YPos,70,aHeigth,'Reservado ao Fisco','',taLeftJustify,True);

     SetFont(FontNameUsed,TamanhoFonte_infComplementares);
     //informacoes complementares
     GotoXY(PosX,PosY+4);
     NewLine;
     NewLine;

     FMemoInfCpl.BaseReport := BaseReport;
     FMemoInfCpl.PrintStart := PosX + 1;
     FMemoInfCpl.PrintEnd   := PosX + wInfCpl - 2;
     FMemoInfCpl.NoNewLine  := True;
     qLin := Trunc( (YFim-YPos) / GetFontHeigh ) - Trunc(((aHeigth*2)/32));
     FMemoInfCpl.PrintLines(qLin,False);

     Result:=PosY;
  end;
end;

function ImprimirRodape(PosX: Double; PosY: Double): Double;
var vEnd:String;
begin
  with DANFeRave, DANFeRave.ACBrNFe.NotasFiscais.Items[DANFeRave.FNFIndex].NFe, DANFeRave.BaseReport do
   begin
     SetFontTitle;
     Result:={FLastY}PosY-GetFontHeigh;
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

     TituloDoBloco(PosX,FirstY,'DADOS DOS PRODUTOS / SERVIÇOS');
     FirstY:=YPos;
     YY:=YPos;
     XX:=XPos;
     Box([],XPos,YPos,FLastX-XPos,LastY-FirstY);
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
     SetFont(FontNameUsed,FontSizeItens);
     Bold:=True;
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
            PrintTab(ColsTitle[i])
         else
            PrintTab(ColsTitle[i]);
     end;
     NewLine;
     for i:=Low(ColsTitleAux) to High(ColsTitleAux) do
     begin
         // #consult atech, o usuário resolve não imprimir o vr. desconto na linha do item
         if (i=4) and (emit.CRT <> crtRegimeNormal)  then
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
            PrintTab(ColsTitleAux[i])
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
       FSerie:=IntToStr(Ide.serie);
       FNumeroNF:=FormatFloat('000000000',Ide.NNF);
       FNumeroNF:=Copy(FNumeroNF,1,3)+'.'+Copy(FNumeroNF,4,3)+'.'+Copy(FNumeroNF,7,3);
       //FEspelho:=Trim(procNFe.nProt)='';
       FEspelho:=false; //funcionalidade de espelho suspensa devido reclamações
       FChaveNFe:=RightStr(infNFe.ID,44);
     end;

    XX:=MarginLeft;YY:=MarginTop;
    Result:=XX;
    if PosicaoCanhoto = 0 then
      YY:=ImprimirCanhoto(XX,YY);
    ImprimirMensagensDeFundo;
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
       if PosicaoCanhoto = 0 then
         FLastItens:=ImprimirRodape(Result,FLastY)
       else
       begin
         FLastItens:=ImprimirCanhoto(Result,FLastY-18);
         FLastItens:=ImprimirRodape(Result,FLastItens);
       end;
       FLastItens:=ImprimirDadosAdicionais(Result,FLastItens,32);
       FLastItens:=ImprimirCalculoISSQN(Result,FLastItens);
     end
    else begin
       FLastItens:=ImprimirRodape(Result, FLastY);

       if (not IsPrintAllInfCpl) then
          if not IsPrintAllProd then
             FLastItens:=ImprimirDadosAdicionais(Result,FLastItens,32)
            else
             FLastItens:=ImprimirDadosAdicionais(Result,FLastItens,FLastItens-YY-4);

    end;

    if not IsPrintAllProd then
       PrepararItens(Result,YY,FLastItens);

  end;
end;

procedure ImprimirItens(PosX:Double);
var qPrinted,j:Integer;
    aDescProduto, vEnd:String;
    Memo:TMemoBuf;
    aMemoHeigthProduto,aFontHeigth:Double;
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
              end;

             if Prod.med.Count > 0 then
              begin
                for j:=0 to Prod.med.Count-1 do
                 begin
                   with Prod.med.Items[j] do
                    begin
                     aDescProduto:=aDescProduto+
                                   ' -LOTE: '+nLote+
                                   ' QTDADE: '+FormatFloat(qLote)+
                                   ' FABR.: '+FormatDate(DateToStr(dFab))+
                                   ' VAL.: '+FormatDate(DateToStr(dVal))+
                                   IfThen(vPMC>0,' PMC: '+FormatFloat(vPMC),'');
                    end;
                 end;
              end;
          end;

          if Trim(infAdProd)>'' then
            aDescProduto:=aDescProduto+#13+StringReplace(infAdProd,';',#13,[rfReplaceAll]);

          Memo:=TMemoBuf.Create;
          try
            Memo.BaseReport := DANFeRave.BaseReport;
            Memo.PrintStart:=PosX+ColsWidth[1]+0.5;
            Memo.PrintEnd:=Memo.PrintStart+ColsWidth[2]-0.5;
            Memo.NoNewLine:=true;
            memo.Text:=aDescProduto;
            //Área utilizada para impressão da Descrição + Informação Adicionais do Produto
            aMemoHeigthProduto:=memo.MemoHeightLeft;

            //Testa se a quantidade de linhas a ser impressa ultrapassa o final do quadro dos itens,
            //se ultrapassar cria uma nova página
            if (YPos+aMemoHeigthProduto)>FLastItens then
              Break
            else if FDetIndex>0 then
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
                   PrintTab(FormatFloat(RoundTo(100-((((Prod.VUnCom*Prod.QCom)-Prod.vDesc)/(Prod.VUnCom*Prod.QCom))*100),-1))+'%')
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

            PrintMemo(Memo,0,false);
          finally
            Memo.Free;
          end;

          // Beretta
          If Prod.UCom <> Prod.uTrib Then
          begin
            NewLine;
            PrintTab('');  // 1
            PrintTab('');  // 2
            PrintTab('');  // 3
            PrintTab('');  // 4
            PrintTab('');  // 5
            PrintTab(Prod.uTrib);  // 6
            PrintTab(FormatFloat(Prod.qTrib,'0.0000'));
            PrintTab(FormatFloat(Prod.vUnTrib,'0.0000'));
          end;

          Inc(FDetIndex);
          NewLine;
        end;
     end;
  end;
end;

procedure ImprimirRetrato(aRaveSystem:TDANFeRave);
var wtemp: double;
    wInfcpl,wInfFisco:String;
    i:Integer;
    bInicio:boolean;
begin
  //tamanho padrao das colunas

  // #consult atech, ajustes no tamanho dos campos código e descrição principalmente
  // e inclusao da coluna valor dos tributos e possibilidade de ocultar a coluna desconto
  ColsWidth[1]:=15;
  ColsWidth[2]:=50;
  ColsWidth[3]:=12;
  ColsWidth[4]:=7;
  ColsWidth[5]:=7;
  ColsWidth[6]:=6;
  ColsWidth[7]:=11;
  ColsWidth[8]:=12;
  ColsWidth[9]:=13;
  ColsWidth[10]:=10;
  ColsWidth[11]:=11;
  ColsWidth[12]:=12;
  ColsWidth[13]:=0;
  ColsWidth[14]:=0;
  ColsWidth[15]:=9;
  ColsWidth[16]:=9;
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
        ColsWidth[ 2]:=ColsWidth[2]+ColsWidth[10];
        ColsWidth[10]:=0;
    end;
    if not ImprimirTributosItem then
    begin
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
