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

unit DoECFObserver;

interface

Uses  Classes, SysUtils ;

Function Parametro( Texto : String; Posicao : Integer ) : String;
Function TraduzObserver( Linhas  : String ) : String ;
Function MudaObserverACBr( Comando : String ) : String ;

implementation

Uses ACBrECFClass,
     {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

Function Parametro(Texto : String; Posicao : Integer ) : String;
Var P, i : Integer ;
begin
  DecimalSeparator := '.';
  Texto := StringReplace( Texto, ',', DecimalSeparator,[rfReplaceAll]) ;

  for i:= 0 to Posicao-1 do
  begin
     P := pos(';',Texto) ;
     if (P > 0) and (i > 0) then
        Texto := Trim(Copy(Texto,P+1,Length(Texto)))
  end;

  P := pos(';',Texto) ;
  if P > 0 then
     Result := Trim(Copy(Texto,1,P-1))
  else
     Result := Trim(Texto) ;

end;

Function TraduzObserver( Linhas  : String ) : String ;
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
     Resposta.Add(MudaObserverACBr(Comandos.Strings[i])) ;
  end ;
  Result := Resposta.Text ;
end ;

Function MudaObserverACBr( Comando : String ) : String ;
Var
    Total, Desconto, Preco : Real ;
    FPG       : TACBrECFFormaPagamento ;
    UltItem   : Integer ;
    Cliente   : TACBrECFConsumidor ;
begin
  Result := '' ;
  {$IFNDEF NOGUI}
   if not FrmACBrMonitor.ACBrECF1.Ativo then
       FrmACBrMonitor.ACBrECF1.Ativar ;
  {$ELSE}
   if not dm.ACBrECF1.Ativo then
       dm.ACBrECF1.Ativar ;
  {$ENDIF}
//***********Métodos de Cupom Fiscal*****************//
  if (Parametro(Comando,1) = '1000') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_ABRECUPOM') then
  begin
     Result := 'ECF.AbreCupom("'+Parametro(Comando,2)+'")' ;
     Exit ;
  end;

  Total := 0 ;
  if (Parametro(Comando,1) = '1001') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_VENDEITEM') then
  begin
     // DARUMA_FI_VendeItem(Str_Codigo, Str_Descricao, Str_Aliquota, Str_Tipo_de_Quantidade, Str_Quantidade, Int_Casas_Decimais, Str_Valor_Unitario, Str_Tipo_de_Desconto, Str_Valor_do_Desconto)

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

     Preco  := StrToFloatDef(Parametro(Comando,8),0);
     Result := 'ECF.VendeItem("'+Parametro(Comando,2)+'", "'+Parametro(Comando,3)+'", "'+Parametro(Comando,4)+'", '+Parametro(Comando,6)+', '+FloatToStr(Preco)+' ,'+FloatToStr(Desconto)+', "'+Parametro(Comando,5)+'" ) ';
     Exit ;
  end;

  if (Parametro(Comando,1) = '1004') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_VENDEITEMDEPARTAMENTO') then
  begin
     //DARUMA_FI_VendeItemDepartamento(Str_Codigo, Str_Descricao, Str_Aliquota, Str_Valor_Unitario, Str_Quantidade, Str_Valor_do_Acrescimo, Str_Valor_do_Desconto, Str_Indice_Departameto,  Str_Unidade_de_Medida)
     Desconto := ((StrToFloatDef(Parametro(Comando,8),0)/Total)*100) ;
     Result := 'ECF.VendeItem("'+Parametro(Comando,2)+'", "'+Parametro(Comando,3)+'", "'+Parametro(Comando,4)+'", '+Parametro(Comando,6)+', '+Parametro(Comando,5)+' ,'+FloatToStr(Desconto)+', "'+Parametro(Comando,10)+'" ) ';
     Exit ;
  end;

  if (Parametro(Comando,1) = '1019') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_VENDEITEMTRESDECIMAIS') then
  begin
     //DARUMA_FI_VendeItemTresDecimais(Str_Codigo, Str_Descricao, Str_Aliquota, Str_Quantidade, Str_Valor_Unitario, Str_Acrescimo_ou_Desconto, Str_Percentual_Acrescimo_ou_Desconto)

     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1014') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_CANCELACUPOM') then
  begin
     Result := 'ECF.CancelaCupom' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1005') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_CANCELAITEMANTERIOR') then
  begin
     //DARUMA_FI_CancelaItemAnterior
     {$IFNDEF NOGUI}
          UltItem := FrmACBrMonitor.ACBrECF1.NumUltItem ;
     {$ELSE}
          UltItem := dm.ACBrECF1.NumUltItem ;
     {$ENDIF}
     Result := 'ECF.CancelaItemVendido( '+IntToStr(UltItem)+' )  ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1006') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_CANCELAITEMGENERICO') then
  begin
     //DARUMA_FI_CancelaItemGenerico(Numero_Item)
     Result := 'ECF.CancelaItemVendido( '+Parametro(Comando,2)+' )  ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1007') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_INICIAFECHAMENTOCUPOM') then
  begin
     //DARUMA_FI_IniciaFechamentoCupom(Str_Acrescimo_ou_Desconto, Str_Tipo_do_Acrescimo_ou_Desconto, Str_Valor_do_Acrescimo_ou_Desconto)
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

  if (Parametro(Comando,1) = '1008') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_EFETUAFORMAPAGAMENTO') then
  begin
     //DARUMA_FI_EfetuaFormaPagamento(Str_Descricao_da_Forma_Pagamento, Str_Valor_da_Forma_Pagamento)
     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ENDIF}
     Result := 'ECF.EfetuaPagamento( "'+FPG.Indice+'" , '+FloatToStr(StrToFloatDef(Parametro(Comando,3),0))+' )' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1009') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_EFETUAFORMAPAGAMENTODESCRICAOFORMA') then
  begin
     //DARUMA_FI_EfetuaFormaPagamentoDescricaoForma(Str_Descricao_da_Forma_Pagamento, Str_Valor_da_Forma_Pagamento, Str_Texto_Livre)
     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ENDIF}

     Result := 'ECF.EfetuaPagamento( "'+FPG.Indice+'", '+FloatToStr(StrToFloatDef(Parametro(Comando,3),0))+', "'+Parametro(Comando,4)+'" )' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1012') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_FECHACUPOMRESUMIDO') then
  begin
     //DARUMA_FI_FechaCupomResumido(Str_Descricao_da_Forma_Pagamento, Str_Mensagem_Promocional)
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

  if (Parametro(Comando,1) = '1011') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_FECHACUPOM') then
  begin
     //DARUMA_FI_FechaCupom(Str_Descricao_da_Forma_Pagamento, Str_Acrescimo_ou_Desconto, Str_Tipo_Acrescimo_ou_Desconto, Str_Valor_Acrescimo_ou_Desconto, Str_Valor_Pago, Str_Mensagem_Promocional)
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

  if (Parametro(Comando,1) = '1010') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_TERMINAFECHAMENTOCUPOM') then
  begin
     // DARUMA_FI_TerminaFechamentoCupom(Str_Mensagem_Promocional)
     Result := 'ECF.FechaCupom( "'+ Parametro(Comando,2) +'" )' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1017') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_ESTORNOFORMASPAGAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1013') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_IDENTIFICACONSUMIDOR') then
  begin
     //DARUMA_FI_IdentificaConsumidor(Str_Nome_do_Consumidor, Str_Endereco, Str_CPF_ou_CNPJ)

     {$IFNDEF NOGUI}
          Cliente := FrmACBrMonitor.ACBrECF1.Consumidor.Create;
          Cliente.AtribuiConsumidor( Parametro(Comando,4), Parametro(Comando,2), Parametro(Comando,3) );
     {$ELSE}
          Cliente := dm.ACBrECF1.Consumidor.Create;
          Cliente.AtribuiConsumidor( Parametro(Comando,4), Parametro(Comando,2), Parametro(Comando,3) );
     {$ENDIF}
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1018') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_EMITIRCUPOMADICIONAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1016') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_USAUNIDADEMEDIDA') then
  begin
     //DARUMA_FI_UsaUnidadeMedida(Str_Unidade_de_Medida)
     Result := 'ECF.ProgramaUnidadeMedida( "'+Parametro(Comando,2)+'" )' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1015') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_AUMENTADESCRICAOITEM') then
  begin
     Result := ' ' ;
     Exit ;
  end;

//***********Métodos de Recebimentos, Não Fiscais e Vinculados*****************//

  if (Parametro(Comando,1) = '1203') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_ABRECOMPROVANTENAOFISCALVINCULADO') then
  begin
     // DARUMA_FI_AbreComprovanteNaoFiscalVinculado (Str_Forma_de_Pagamento, Str_Valor_Pago, Str_Numero_do_Cupom)
     {$IFNDEF NOGUI}
        FPG := FrmACBrMonitor.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ELSE}
        FPG := dm.ACBrECF1.AchaFPGDescricao( Parametro(Comando,2), True ) ;
     {$ENDIF}

     Result := 'ECF.AbreCupomVinculado( '+ Parametro(Comando,4) +' , '+FPG.Indice+', '+ Parametro(Comando,3) +' )' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1204') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_USACOMPROVANTENAOFISCALVINCULADO') then
  begin
     //DARUMA_FI_UsaComprovanteNaoFiscalVinculado(Str_Texto_Livre)
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1205') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_FECHACOMPROVANTENAOFISCALVINCULADO') then
  begin
     //DARUMA_FI_FechaComprovanteNaoFiscalVinculado()
     Result := 'ECF.FechaRelatorio' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1208') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_ABRERELATORIOGERENCIAL') then
  begin
     //DARUMA_FI_AbreRelatorioGerencial()
     Result := 'ECF.AbreRelatorioGerencial' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1201') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_FECHARELATORIOGERENCIAL') then
  begin
     //DARUMA_FI_FechaRelatorioGerencial()
     Result := 'ECF.FechaRelatorio' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1210') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_ABRERECEBIMENTONAOFISCAL') then
  begin
     //DARUMA_FI_AbreRecebimentoNaoFiscal(Str_Descricao_do_Totalizador, Str_Acrescimo_ou_Desconto, Str_Tipo_Acrescimo_ou_Deconto, Str_Valor_Acrescimo_ou_Desconto, Str_Valor_do_Recebimento, Str_Texto_Livre)
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1211') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_EFETUAFORMAPAGAMENTONAOFISCAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1212') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_FUNDOCAIXA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1102') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_LEITURAMEMORIAFISCALDATA') then
  begin
     //DARUMA_FI_LeituraMemoriaFiscalData(Str_Data_Inicial, Str_Data_Final)
     Result := 'ECF.LeituraMemoriaFiscal('+ Parametro(Comando,2) +','+ Parametro(Comando,3) +')' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1103') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_LEITURAMEMORIAFISCALREDUCAO') then
  begin
     //DARUMA_FI_LeituraMemoriaFiscalReducao (Str_Reducao_Inicial, Str_Reducao_Final)
     Result := 'ECF.LeituraMemoriaFiscal('+ Parametro(Comando,2) +','+ Parametro(Comando,3) +')' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1104') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_LEITURAMEMORIAFISCALSERIALDATA') then
  begin
     // DARUMA_FI_LeituraMemoriaFiscalSerialData (Str_Data_Inicial, Str_Data_Final)
     Result := 'ECF.LeituraMemoriaFiscalSerial('+ Parametro(Comando,2) +','+ Parametro(Comando,3) +')' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1105') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_LEITURAMEMORIAFISCALSERIALREDUCAO') then
  begin
     //DARUMA_FI_LeituraMemoriaFiscalSerialReducao (Str_Reducao_Inicial, Str_Reducao_Final)
     Result := 'ECF.LeituraMemoriaFiscalSerial('+ Parametro(Comando,2) +','+ Parametro(Comando,3) +')' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1101') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_LEITURAX') then
  begin
     //DARUMA_FI_LeituraX()
     Result := 'ECF.LeituraX' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1202') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_RECEBIMENTONAOFISCAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1100') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_REDUCAOZ') then
  begin
     // DARUMA_FI_ReducaoZ(" ", " ")
     Result := 'ECF.ReducaoZ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1106') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_REDUCAOZAJUSTADATAHORA') then
  begin
     // DARUMA_FI_ReducaoZAjustaDataHora(Str_Data, Str_Hora)   - Acrescentar data e hora
     Result := 'ECF.ReducaoZ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1200') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_RELATORIOGERENCIAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1206') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_SANGRIA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1207') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_SUPRIMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

//***********Métodos Gaveta, Autenticação e Outras*****************//
  if (Parametro(Comando,1) = '1301') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_VERIFICADOCAUTENTICACAO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1300') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_AUTENTICACAO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1302') or (UpperCase(Parametro(Comando,1)) = ' DARUMA_FI_AUTENTICACAOSTR') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1311') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_VERIFICAESTADOGAVETA') then
  begin
     //D aruma_FI_VerificaEstadoGaveta( Int_Estado_Gaveta);
     Result := 'ECF.GavetaAberta' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1312') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_VERIFICAESTADOGAVETASTR') then
  begin
     // DARUMA_FI_VerificaEstadoGavetaStr(Str_Estado_Gaveta)
     Result := 'ECF.GavetaAberta' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1310') or (UpperCase(Parametro(Comando,1)) = 'DARUMA_FI_ACIONAGAVETA') then
  begin
     //DARUMA_FI_AcionaGaveta()
     Result := 'ECF.Abregaveta' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '600') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_ABREPORTASERIAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '601') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_FECHAPORTASERIAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '603') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_ABERTURADODIA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '604') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_FECHAMENTODODIA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '607') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_IMPRIMECONFIGURACOESIMPRESSORA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

//***********Métodos Programação e Configuração*****************//
  if (Parametro(Comando,1) = '302') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PROGRAMAALIQUOTA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '304') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NOMEIATOTALIZADORNAOSUJEITOICMS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '301') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PROGRAMAFORMASPAGAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '310') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PROGRAMAOPERADOR') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '305') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PROGRAMAARREDONDAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '306') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PROGRAMATRUNCAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '307') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_LINHASENTRECUPONS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '308') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_ESPACOENTRELINHAS ') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '303') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PROGRAMAHORARIOVERAO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '???') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_EQUALIZAFORMASPGTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '312') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PROGRAMAVINCULADOS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '313') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PROGRAMAPGTOSEMVINCULAR ') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '400') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGFECHAAUTOMATICOCUPOM') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '401') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGREDZAUTOMATICO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '411') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGIMPESTGAVVENDAS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '402') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGLEITURAXAUTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '403') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGCALCARREDONDAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '404') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGHORARIOVERAO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '405') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGSENSORAUT') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '412') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGCUPOMADICIONAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '407') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGESPACAMENTOCUPONS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '408') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGHORAMINREDUCAOZ') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '409') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGLIMIARNEAREND') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '410') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CFGPERMMENSPROMOCNF') then
  begin
     Result := ' ' ;
     Exit ;
  end;

//***********Métodos de Informação, Status e Retorno*****************//
  if (Parametro(Comando,1) = '1404') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_STATUSCUPOMFISCAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1405') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_STATUSRELATORIOGERENCIAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1403') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_STATUSCOMPROVANTENAOFISCALVINCULADO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1471') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_STATUSCOMPROVANTENAOFISCALNAOVINCULADO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1400') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAIMPRESSORALIGADA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1469') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAMODELOECF') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1500') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAHORARIOVERAO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1484') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICADIAABERTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1489') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAZPENDENTE') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1488') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAXPENDENTE') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1444') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICATIPOIMPRESSORA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1470') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICADESCRICAOFORMASPAGAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1442') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAFORMASPAGAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1448') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAFORMASPAGAMENTOEX') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1401') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAESTADOIMPRESSORA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1440') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAALIQUOTASISS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1445') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAINDICEALIQUOTASISS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1436') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICATOTALIZADORESNAOFISCAIS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1486') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICATOTALIZADORESNAOFISCAISEX') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1432') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAEPROMCONECTADA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1443') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICARECEBIMENTONAOFISCAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1439') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICATRUNCAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1431') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICAMODOOPERACAO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1407') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICATOTALIZADORESPARCIAIS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1424') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CLICHEPROPRIETARIO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1467') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CLICHEPROPRIETARIOEX') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1425') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROCAIXA') then
  begin
     //DARUMA_FI_NumeroCaixa(Str_Informacao)
     Result := 'ECF.NumECF' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1426') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROLOJA') then
  begin
     //DARUMA_FI_NumeroLoja(Str_Informacao)
     Result := 'ECF.NumLoja' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1411') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROSERIE') then
  begin
     //DARUMA_FI_NumeroSerie(Str_Informacao)
     Result := 'ECF.NumSerie' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1491') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_REGISTRANUMEROSERIE') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1492') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERIFICANUMEROSERIE') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '????') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNASERIALCRIPTOGRAFADO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1412') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VERSAOFIRMWARE') then
  begin
     //DARUMA_FI_VersaoFirmware(Str_Informacao)
     Result := 'ECF.NumVersao' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1413') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CGC_IE') then
  begin
     //DARUMA_FI_CGC_IE(Str_CGC, Str_IE)
     Result := 'ECF.CNPJ'+sLineBreak+'ECF.IE' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1417') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROCUPOM') then
  begin
     //DARUMA_FI_NumeroCupom(Str_Informacao)
     Result := 'ECF.NumCupom' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1468') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_COO') then
  begin
     //
     Result := 'ECF.NumCRO' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1430') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_MINUTOSIMPRIMINDO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1429') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_MINUTOSLIGADA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1422') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROSUBSTITUICOESPROPRIETARIO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1421') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROINTERVENCOES') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1420') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROREDUCOES') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1419') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROCUPONSCANCELADOS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1418') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_NUMEROOPERACOESNAOFISCAIS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1434') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_DATAHORAIMPRESSORA') then
  begin
     // DARUMA_FI_DataHoraImpressora(Str_Data, Str_Hora)
     Result := 'ECF.DataHora' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1437') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_DATAHORAREDUCAO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1438') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_DATAMOVIMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1435') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CONTADORESTOTALIZADORESNAOFISCAIS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1483') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_LERALIQUOTASCOMINDICE') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1490') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VENDABRUTA') then
  begin
     // DARUMA_FI_VendaBruta(Str_Informacao)
     Result := 'ECF.VendaBruta' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1498') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VENDABRUTAACUMULADA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1414') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_GRANDETOTAL') then
  begin
     // DARUMA_FI_GrandeTotal(Str_Informacao)
     Result := 'ECF.GrandeTotal' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1415') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_DESCONTOS') then
  begin
     // DARUMA_FI_Descontos(Str_Informacao)
     Result := 'ECF.TotalDescontos' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1441') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_ACRESCIMOS') then
  begin
     // DARUMA_FI_Descontos(Str_Informacao)
     Result := 'ECF.TotalAcrescimos' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1416') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_CANCELAMENTOS') then
  begin
     // DARUMA_FI_Cancelamentos(Str_Informacao)
     Result := 'ECF.TotalCancelamentos' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1410') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_DADOSULTIMAREDUCAO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1408') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_SUBTOTAL') then
  begin
     // DARUMA_FI_SubTotal(Str_Informacao)
     Result := 'ECF.SubTotal' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1450') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_TROCO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1449') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_SALDOAPAGAR') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1406') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNOALIQUOTAS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1433') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VALORPAGOULTIMOCUPOM') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1473') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_ULTIMAFORMAPAGAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1446') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VALORFORMAPAGAMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1447') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_VALORTOTALIZADORNAOFISCAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1423') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_ULTIMOITEMVENDIDO') then
  begin
     //DARUMA_FI_UltimoItemVendido(Str_Informacao)
     Result := 'ECF.NumUltItem' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1493') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_TIPOULTIMODOCUMENTO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '602') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_MAPARESUMO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '605') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RELATORIOTIPO60ANALITICO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '606') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RELATORIOTIPO60MESTRE') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1428') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_FLAGSFISCAIS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1481') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_PALAVRASTATUS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1479') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_FLAGSFISCAISSTR') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1427') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_SIMBOLOMOEDA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1402') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNOIMPRESSORA') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1472') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNOERROEXTENDIDO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1451') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNAACRESCIMONF') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1452') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACFCANCELADOS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1453') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACNFCANCELADOS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1454') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACLX') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1455') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACNFNV') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1456') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACNFV') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1497') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNADescricaoCNFV') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1457') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACRO') then
  begin
     // DARUMA_FI_RetornaCRO(Str_Informacao)
     Result := 'ECF.NumCRO' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1458') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACRZ') then
  begin
     //DARUMA_FI_RetornaCRZ(Str_Informacao)
     Result := 'ECF.NumCRZ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1459') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACRZRestante') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1460') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNACANCELAMENTONF') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1461') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNADESCONTONF') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1462') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNAGNF') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1463') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNATEMPOIMPRIMINDO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1464') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNATEMPOLIGADO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1465') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNATOTALPAGAMENTOS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1466') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNATROCO') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1499') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNAREGISTRADORESNAOFISCAIS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1485') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNAREGISTRADORESFISCAIS') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1495') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNAVALORCOMPROVANTENAOFISCAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1496') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNAINDICECOMPROVANTENAOFISCAL') then
  begin
     Result := ' ' ;
     Exit ;
  end;

  if (Parametro(Comando,1) = '1502') or (UpperCase(Parametro(Comando,1)) =  'DARUMA_FI_RETORNARVERSAODLL') then
  begin
     Result := ' ' ;
     Exit ;
  end;


end;

end.
