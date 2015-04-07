{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

{$mode objfpc}{$H+}

unit DoECFBemafi32;

interface

Uses  Classes, SysUtils ;

Function Parametro( Texto : String; Posicao : Integer ) : String;
Function TraduzBemafi( Linhas  : String ) : String ;
Function MudaBemaACBr( Comando : String ) : String ;

implementation

Uses ACBrECFClass,
     {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF}  ;

Function Parametro(Texto : String; Posicao : Integer ) : String;
Var P, i : Integer ;
begin
  DecimalSeparator := '.';

  for i:= 0 to Posicao-1 do
  begin
     P := pos('|',Texto) ;
     if (P > 0) and (i > 0) then
        Texto := Trim(Copy(Texto,P+1,Length(Texto)))
  end;

  P := pos('|',Texto) ;
  if P > 0 then
     Result := Trim(Copy(Texto,1,P-1))
  else
     Result := Trim(Texto) ;

end;

Function TraduzBemafi( Linhas : String ) : String ;
Var i        : Integer ;
    Resposta : TStringList ;
    Comandos : TStringList ;

begin
  Comandos := TStringList.Create ;
  Resposta := TStringList.Create ;

  Comandos.Text := Linhas ;
  Resposta.Clear ;

  for i:=0 to Comandos.Count-1 do
  begin
     Resposta.Add(MudaBemaACBr(Comandos.Strings[i])) ;
  end ;
  Result := Resposta.Text ;
end ;

Function MudaBemaACBr( Comando : String ) : String ;
Var
    Total, Desconto : Real ;
    FPG       : TACBrECFFormaPagamento ;
    CNF       : TACBrECFComprovanteNaoFiscal ;
    UltItem   : Integer ;
    Cliente   : TACBrECFConsumidor ;
    COO       : String ;
begin
  Result := '' ;
  {$IFNDEF NOGUI}
   if not FrmACBrMonitor.ACBrECF1.Ativo then
       FrmACBrMonitor.ACBrECF1.Ativar ;
  {$ELSE}
   if not dm.ACBrECF1.Ativo then
       dm.ACBrECF1.Ativar ;
  {$ENDIF}

//***********Comandos de Cupom Fiscal*****************//
  if (Parametro(Comando,1) = '003') or (Parametro(Comando,1) = '110')then
  begin
     // Bematech_FI_AbreCupom ou
     // Bematech_FI_AbreCupomMFD 110|10.123.154-98|José da Silva|R. Sem Fim, 1000| 
     if Parametro(Comando,1) = '110' then
     begin
        {$IFNDEF NOGUI}
             Cliente := FrmACBrMonitor.ACBrECF1.Consumidor.Create;
             Cliente.AtribuiConsumidor( Parametro(Comando,2), Parametro(Comando,3), Parametro(Comando,4)  );
        {$ELSE}
             Cliente := dm.ACBrECF1.Consumidor.Create;
             Cliente.AtribuiConsumidor( Parametro(Comando,2), Parametro(Comando,3), Parametro(Comando,4) );
        {$ENDIF}
     end ;
     Result := 'ECF.AbreCupom("'+Parametro(Comando,2)+'")' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '009' then
  begin
     //Bematech_FI_CancelaCupom
     Result := 'ECF.CancelaCupom' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '011' then
  begin
     //Bematech_FI_CancelaItemAnterior
     {$IFNDEF NOGUI}
          UltItem := FrmACBrMonitor.ACBrECF1.NumUltItem ;
     {$ELSE}
          UltItem := dm.ACBrECF1.NumUltItem ;
     {$ENDIF}
     Result := 'ECF.CancelaItemVendido( '+IntToStr(UltItem)+' )  ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '012' then
  begin
     //Bematech_FI_CancelaItemGenerico
     Result := 'ECF.CancelaItemVendido( '+Parametro(Comando,2)+' )  ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '023' then
  begin
     //Bematech_FI_EfetuaFormaPagamento 023|Dinheiro|25,00|
     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ENDIF}
     Result := 'ECF.EfetuaPagamento( "'+FPG.Indice+'" , '+FloatToStr(StrToFloatDef(Parametro(Comando,3),0)/100)+' )' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '024' then
  begin
     //Bematech_FI_EfetuaFormaPagamentoDescricaoForma  024|Cheque|50,00|Bom p/ 30 dias|
     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ENDIF}

     Result := 'ECF.EfetuaPagamento( "'+FPG.Indice+'", '+FloatToStr(StrToFloatDef(Parametro(Comando,2),0)/100)+', "'+Parametro(Comando,3)+'" )' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '026' then
  begin
     //Bematech_FI_EstornoFormasPagamento
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '028' then
  begin
     //Bematech_FI_FechaCupom  028|Dinheiro|A|$|0000|35,00|Obrigado, volte sempre !!!| 
     // Tem q Subtotalizar com desconto ou acréscimo e efetuar pagamento
     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ENDIF}

     if Parametro(Comando,3) = 'D' then
     begin
        if Parametro(Comando,4) = '$' then
           Result := 'ECF.SubtotalizaCupom( -'+ Parametro(Comando,5) +' )'
        else
        begin
           {$IFNDEF NOGUI}
               Total := FrmACBrMonitor.ACBrECF1.Subtotal ;
           {$ELSE}
               Total := dm.ACBrECF1.Subtotal ;
           {$ENDIF}
           Result := 'ECF.SubtotalizaCupom( -'+ FloatToStr(Total*(StrToFloat(Parametro(Comando,5))/100)) +' )'
        end;
     end
     else
     begin
        if Parametro(Comando,4) = '%' then
           Result := 'ECF.SubtotalizaCupom( '+ Parametro(Comando,5) +' )'
        else
        begin
           {$IFNDEF NOGUI}
               Total := FrmACBrMonitor.ACBrECF1.Subtotal ;
           {$ELSE}
               Total := dm.ACBrECF1.Subtotal ;
           {$ENDIF}
           Result := 'ECF.SubtotalizaCupom( '+ FloatToStr(Total*(StrToFloat(Parametro(Comando,5))/100)) +' )'
        end;

     end;

     Result := Result +sLineBreak+'ECF.EfetuaPagamento( "'+FPG.Indice+'" , '+FloatToStr(StrToFloatDef(Parametro(Comando,6),0)/100)+' )' ;

     Result := Result +sLineBreak+'ECF.FechaCupom( "'+Parametro(Comando,7)+'" )' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '029' then
  begin
     // Bematech_FI_FechaCupomResumido 029|Dinheiro|Obrigado, volte sempre !!!|
     // Tem q efetuar pagamento
     {$IFNDEF NOGUI}
         Total := FrmACBrMonitor.ACBrECF1.Subtotal ;
     {$ELSE}
         Total := dm.ACBrECF1.Subtotal ;
     {$ENDIF}

     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ENDIF}
     Result := 'ECF.SubtotalizaCupom' ;

     Result := Result +sLineBreak+'ECF.EfetuaPagamento( "'+FPG.Indice+'" , '+FloatToStr(Total)+' )' ;

     Result := Result +sLineBreak+'ECF.FechaCupom( "'+Parametro(Comando,3)+'" )' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '040' then
  begin
     //Bematech_FI_IniciaFechamentoCupom 040|D|%|10,00| 
     if Parametro(Comando,2) = 'D' then
     begin
        if Parametro(Comando,3) = '$' then
           Result := 'ECF.SubtotalizaCupom( -'+ Parametro(Comando,3) +' )'
        else
        begin
           {$IFNDEF NOGUI}
               Total := FrmACBrMonitor.ACBrECF1.Subtotal ;
           {$ELSE}
               Total := dm.ACBrECF1.Subtotal ;
           {$ENDIF}
           Result := 'ECF.SubtotalizaCupom( -'+ FloatToStr(StrToFloatDef(Parametro(Comando,4),0)) +' )'
        end;
     end
     else
     begin
        if Parametro(Comando,3) = '$' then
           Result := 'ECF.SubtotalizaCupom( '+ Parametro(Comando,4) +' )'
        else
        begin
           {$IFNDEF NOGUI}
               Total := FrmACBrMonitor.ACBrECF1.Subtotal ;
           {$ELSE}
               Total := dm.ACBrECF1.Subtotal ;
           {$ENDIF}
           Result := 'ECF.SubtotalizaCupom( '+ FloatToStr(Total*(StrToFloat(Parametro(Comando,4))/100)) +' )'
        end;

     end;
     Exit ;
  end;

  if Parametro(Comando,1) = '082' then
  begin
     // Bematech_FI_TerminaFechamentoCupom  082|Obrigado, volte sempre !!!|
     Result := 'ECF.FechaCupom( "'+ Parametro(Comando,2) +'" )' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '085' then
  begin
     //Bematech_FI_UsaUnidadeMedida 085|Kg|
     Result := 'ECF.ProgramaUnidadeMedida( "'+Parametro(Comando,2)+'" )' ;
     Exit ;
  end;

  Total := 0 ;
  if Parametro(Comando,1) = '089' then
  begin
     //Bematech_FI_VendeItem
     if StrToFloat(Parametro(Comando,10)) > 0 then
     begin
        if Parametro(Comando,9) = '%' then
        begin
           {$IFNDEF NOGUI}
               Total := FrmACBrMonitor.ACBrECF1.Subtotal ;
           {$ELSE}
               Total := dm.ACBrECF1.Subtotal ;
           {$ENDIF}
           Desconto := StrToFloatDef(Parametro(Comando,10),0) ;
        end
        else
           Desconto := ((StrToFloatDef(Parametro(Comando,10),0)/Total)*100) ;
     end
     else
        Desconto := 0 ;
     Result := 'ECF.VendeItem("'+Parametro(Comando,2)+'", "'+Parametro(Comando,3)+'", "'+Parametro(Comando,4)+'", '+Parametro(Comando,6)+', '+FloatToStr(StrToFloatDef(Parametro(Comando,8),0)/100)+' ,'+FloatToStr(Desconto)+', "'+Parametro(Comando,5)+'" ) ';
     Exit ;
  end;

  if Parametro(Comando,1) = '090' then
  begin
     //Bematech_FI_VendeItemDepartamento
     Desconto := ((StrToFloatDef(Parametro(Comando,8),0)/Total)*100) ;
     Result := 'ECF.VendeItem("'+Parametro(Comando,2)+'", "'+Parametro(Comando,3)+'", "'+Parametro(Comando,4)+'", '+Parametro(Comando,6)+', '+Parametro(Comando,5)+' ,'+FloatToStr(Desconto)+', "'+Parametro(Comando,10)+'" ) ';
     Exit ;
  end;

  if Parametro(Comando,1) = '260' then
  begin
     //Bematech_FI_EfetuaFormaPagamentoIndice
     Result := 'ECF.EfetuaPagamento( "'+Parametro(Comando,2)+'",'+FloatToStr(StrToFloatDef(Parametro(Comando,3),0)/100)+')' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '281' then
  begin
     //Bematech_FI_EfetuaFormaPagamentoIndiceDescricaoForma
     Result := 'ECF.EfetuaPagamento( "'+Parametro(Comando,2)+'",'+FloatToStr(StrToFloatDef(Parametro(Comando,3),0)/100)+',"'+Parametro(Comando,4)+'")' ;
     Exit ;
  end;

//***********Comandos de Operações Não Fiscais*****************//
  if Parametro(Comando,1) = '002' then
  begin
     //Bematech_FI_AbreComprovanteNaoFiscalVinculado    002|Cartao|||
     if Length(Parametro(Comando,2)) <= 0 then
     begin
        {$IFNDEF NOGUI}
           COO := FrmACBrMonitor.ACBrECF1.NumCOO ;
        {$ELSE}
           COO := dm.ACBrECF1.NumCOO ;
        {$ENDIF}
     end
     else
        COO := Parametro(Comando,2) ;

     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ENDIF}

     Result := 'ECF.AbreCupomVinculado( '+ COO +' , '+FPG.Indice+' )' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '027' then
  begin
     //Bematech_FI_FechaComprovanteNaoFiscalVinculado 027|
     Result := 'ECF.FechaRelatorio' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '031' then
  begin
     //Bematech_FI_FechaRelatorioGerencial 031|
     Result := 'ECF.FechaRelatorio' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '070' then
  begin
     //Bematech_FI_RecebimentoNaoFiscal 070|05-IndiceTotalizador|30,00-ValorRecebimento|Dinheiro-FormaPagamento|

     {$IFNDEF NOGUI}
        CNF := FrmACBrMonitor.ACBrECF1.AchaCNFIndice( Parametro(Comando,2) ) ;
     {$ELSE}
        CNF := dm.ACBrECF1.AchaCNFIndice( Parametro(Comando,2) ) ;
     {$ENDIF}

     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGIndice( Parametro(Comando,4) ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGIndice( Parametro(Comando,4) ) ;
     {$ENDIF}

     Result := 'ECF.NaoFiscalCompleto('+  CNF.Indice + ',' + Parametro(Comando,3) +','+ FPG.Indice +')' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '072' then
  begin
     //Bematech_FI_RelatorioGerencial  072|Digite o texto a ser impresso aqui !!!|
     Result := 'ECF.RelatorioGerencial('+ Parametro(Comando,2) +')' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '078' then
  begin
     //Bematech_FI_Sangria  078|50,00|
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '081' then
  begin
     //Bematech_FI_Suprimento 081|100,00|Dinheiro|
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '084' then
  begin
     //Bematech_FI_UsaComprovanteNaoFiscalVinculado 084|Digite o texto a ser impresso aqui !!!|
     Result := ' ' ;
     Exit ;
  end;

//***********Comandos de Relatórios Fiscais*****************//
  if Parametro(Comando,1) = '045' then
  begin
     //Bematech_FI_LeituraX 045|
     Result := 'ECF.LeituraX' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '046' then
  begin
     //Bematech_FI_LeituraXSerial  046|
     {$IFNDEF NOGUI}
        COO := FrmACBrMonitor.ACBrECF1.NumCRZ ;
     {$ELSE}
        COO := dm.ACBrECF1.NumCRZ ;
     {$ENDIF}
//     Result := 'ECF.LeituraMemoriaFiscalSerial('+COO+','+COO+')' ;
     Result := '' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '041' then
  begin
     //Bematech_FI_LeituraMemoriaFiscalData 041|01/01/2002|05/01/2001|
     Result := 'ECF.LeituraMemoriaFiscal('+ Parametro(Comando,2) +','+ Parametro(Comando,3) +')' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '042' then
  begin
     //Bematech_FI_LeituraMemoriaFiscalReducao 042|0100|0250|
     Result := 'ECF.LeituraMemoriaFiscal('+ Parametro(Comando,2) +','+ Parametro(Comando,3) +')' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '043' then
  begin
     //Bematech_FI_LeituraMemoriaFiscalSerialData  043|01/01/2002|05/01/2001|
     Result := 'ECF.LeituraMemoriaFiscalSerial('+ Parametro(Comando,2) +','+ Parametro(Comando,3) +')' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '044' then
  begin
     //Bematech_FI_LeituraMemoriaFiscalSerialReducao  044|0100|0250|
     Result := 'ECF.LeituraMemoriaFiscalSerial('+ Parametro(Comando,2) +','+ Parametro(Comando,3) +')' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '071' then
  begin
     //Bematech_FI_ReducaoZ 071|
     Result := 'ECF.ReducaoZ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '238' then
  begin
     //Bematech_FI_FechaRelatorioXouZ
     Result := 'ECF.FechaRelatorio' ;
     Exit ;
  end;

//***********Comandos de Autenticação e Gaveta de Dinheiro*****************//
  if Parametro(Comando,1) = '004' then
  begin
     //Bematech_FI_AcionaGaveta 004|
     Result := 'ECF.Abregaveta' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '008' then
  begin
     //Bematech_FI_Autenticacao
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '065' then
  begin
     //Bematech_FI_ProgramaCaracterAutenticacao 065|001,002,004,008,016,032,064,128,064,032,016,008,004,002,129,129,129,129|
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '094' then
  begin
     //Bematech_FI_VerificaEstadoGaveta 094|
     Result := 'ECF.GavetaAberta' ;
     Exit ;
  end;

//***********Comandos de Inicialização*****************//
  if Parametro(Comando,1) = '006' then
  begin
     //Bematech_FI_AlteraSimboloMoeda  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '025' then
  begin
     //Bematech_FI_EspacoEntreLinhas
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '033' then
  begin
     //Bematech_FI_ForcaImpactoAgulhas  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '047' then
  begin
     //Bematech_FI_LinhasEntreCupons
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '053' then
  begin
     //Bematech_FI_NomeiaTotalizadorNaoSujeitoIcms  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '063' then
  begin
     //Bematech_FI_ProgramaAliquota  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '064' then
  begin
     //Bematech_FI_ProgramaArredondamento
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '066' then
  begin
     //Bematech_FI_ProgramaHorarioVerao  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '069' then
  begin
     //Bematech_FI_ProgramaTruncamento
     Result := ' ' ;
     Exit ;
  end;


//***********Comandos de Informações da Impressora*****************//
  if Parametro(Comando,1) = '005' then
  begin
     //Bematech_FI_Acrescimos
     Result := 'ECF.TotalAcrescimos ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '013' then
  begin
     //Bematech_FI_Cancelamentos
     Result := 'ECF.TotalCancelamentos' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '014' then
  begin
     //Bematech_FI_CGC_IE
     Result := 'ECF.CNPJ'+sLineBreak+'ECF.IE' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '015' then
  begin
     //Bematech_FI_ClicheProprietario
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '017' then
  begin
     //Bematech_FI_ContadoresTotalizadoresNaoFiscais
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '018' then
  begin
     //Bematech_FI_DadosUltimaReducao
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '019' then
  begin
     //Bematech_FI_DataHoraImpressora 
     Result := 'ECF.DataHora' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '020' then
  begin
     //Bematech_FI_DataHoraReducao  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '021' then
  begin
     //Bematech_FI_DataMovimento
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '022' then
  begin
     //Bematech_FI_Descontos
     Result := 'ECF.TotalDescontos' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '032' then
  begin
     //Bematech_FI_FlagsFiscais
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '034' then
  begin
     //Bematech_FI_GrandeTotal 
     Result := 'ECF.GrandeTotal' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '050' then
  begin
     //Bematech_FI_MinutosImprimindo
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '049' then
  begin
     //Bematech_FI_MinutosLigada
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '051' then
  begin
     //Bematech_FI_MonitoramentoPapel  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '054' then
  begin
     //Bematech_FI_NumeroCaixa
     Result := 'ECF.NumECF' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '055' then
  begin
     //Bematech_FI_NumeroCupom
     Result := 'ECF.NumCupom' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '056' then
  begin
     //Bematech_FI_NumeroCuponsCancelados
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '057' then
  begin
     //Bematech_FI_NumeroIntervencoes
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '058' then
  begin
     //Bematech_FI_NumeroLoja 
     Result := 'ECF.NumLoja' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '059' then
  begin
     //Bematech_FI_NumeroOperacoesNaoFiscais 
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '060' then
  begin
     //Bematech_FI_NumeroReducoes 
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '061' then
  begin
     //Bematech_FI_NumeroSerie 
     Result := 'ECF.NumSerie' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '062' then
  begin
     //Bematech_FI_NumeroSubstituicoesProprietario
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '076' then
  begin
     //Bematech_FI_RetornoAliquotas
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '079' then
  begin
     //Bematech_FI_SimboloMoeda  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '080' then
  begin
     //Bematech_FI_SubTotal
     Result := 'ECF.SubTotal' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '083' then
  begin
     //Bematech_FI_UltimoItemVendido
     Result := 'ECF.NumUltItem' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '086' then
  begin
     //Bematech_FI_ValorFormaPagamento  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '088' then
  begin
     //Bematech_FI_ValorPagoUltimoCupom
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '087' then
  begin
     //Bematech_FI_ValorTotalizadorNaoFiscal 
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '091' then
  begin
     //Bematech_FI_VerificaAliquotasIss
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '093' then
  begin
     //Bematech_FI_VerificaDepartamentos 
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '092' then
  begin
     //Bematech_FI_VerificaEpromConectada
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '095' then
  begin
     //Bematech_FI_VerificaEstadoImpressora
     Result := 'ECF.Estado ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '096' then
  begin
     //Bematech_FI_VerificaFormasPagamento
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '098' then
  begin
     //Bematech_FI_VerificaIndiceAliquotasIss  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '099' then
  begin
     //Bematech_FI_VerificaModoOperacao
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '100' then
  begin
     //Bematech_FI_VerificaRecebimentoNaoFiscal 
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '102' then
  begin
     //Bematech_FI_VerificaTipoImpressora
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '103' then
  begin
     //Bematech_FI_VerificaTotalizadoresNaoFiscais
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '104' then
  begin
     //Bematech_FI_VerificaTotalizadoresParciais  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '105' then
  begin
     //Bematech_FI_VerificaTruncamento
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '106' then
  begin
     //Bematech_FI_VersaoFirmware
     Result := 'ECF.NumVersao' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '262' then
  begin
     //Bematech_FI_FlagsVinculacaoIss 
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '263' then
  begin
     //Bematech_FI_VerificaReducaoZAutomatica 
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '266' then
  begin
     //Bematech_FI_ModeloImpressora
     Result := 'ECF.Modelo' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '279' then
  begin
     //Bematech_FI_NumeroSerieCriptografado
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '280' then
  begin
     //Bematech_FI_NumeroSerieDescriptografado
     Result := ' ' ;
     Exit ;
  end;

//***********Comandos de Impressão de Cheques*****************//
  if Parametro(Comando,1) = '010' then
  begin
     //Bematech_FI_CancelaImpressaoCheque
     Result := 'ECF.CancelaImpressaoCheque' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '035' then
  begin
     //Bematech_FI_ImprimeCheque  035|353|50,00|André|Curitiba|10/01/02|Bom p/ 30 dias|
     Result := 'ECF.ImprimeCheque( '+ Parametro(Comando,2) +', '+ Parametro(Comando,3) +', '+ Parametro(Comando,4) +', '+ Parametro(Comando,5) +', '+ Parametro(Comando,6) +' , '+ Parametro(Comando,7) +' ) ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '037' then
  begin
     //Bematech_FI_ImprimeCopiaCheque
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '039' then
  begin
     //Bematech_FI_IncluiCidadeFavorecido
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '067' then
  begin
     //Bematech_FI_ProgramaMoedaPlural
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '068' then
  begin
     //Bematech_FI_ProgramaMoedaSingular
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '101' then
  begin
     //Bematech_FI_VerificaStatusCheque 
     Result := 'ECF.ChequePronto' ;
     Exit ;
  end;

//***********Comandos de TEF*****************//
  if Parametro(Comando,1) = '198' then
  begin
     //Bematech_FI_IniciaModoTEF
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '200' then
  begin
     //Bematech_FI_UsaComprovanteNaoFiscalVinculadoTEF
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '201' then
  begin
     //Bematech_FI_RelatorioGerencialTEF
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '199' then
  begin
     //Bematech_FI_FinalizaModoTEF
     Result := ' ' ;
     Exit ;
  end;

//***********Outros Comandos da Impressora *****************//
  if Parametro(Comando,1) = '001' then
  begin
     //Bematech_FI_AberturaDoDia
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '030' then
  begin
     //Bematech_FI_FechamentoDoDia  

     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '036' then
  begin
     //Bematech_FI_ImprimeConfiguracoesImpressora  
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '038' then
  begin
     //Bematech_FI_ImprimeDepartamentos
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '048' then
  begin
     //Bematech_FI_MapaResumo
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '073' then
  begin
     //Bematech_FI_RelatorioTipo60Analitico
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '074' then
  begin
     //Bematech_FI_RelatorioTipo60Mestre
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '075' then
  begin
     //Bematech_FI_ResetaImpressora
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '077' then
  begin
     //Bematech_FI_RetornoImpressora
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '097' then
  begin
     //Bematech_FI_VerificaImpressoraLigada
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '195' then
  begin
     //Bematech_FI_DadosSintegra
     Result := ' ' ;
     Exit ;
  end;

  if Parametro(Comando,1) = '197' then
  begin
     //Bematech_FI_RegistrosTipo60 
     Result := ' ' ;
     Exit ;
  end;


end;


end.
