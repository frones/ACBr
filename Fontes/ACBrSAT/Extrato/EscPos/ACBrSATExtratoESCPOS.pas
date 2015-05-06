{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 04/04/2013:  André Ferreira de Moraes
|*   Inicio do desenvolvimento
******************************************************************************}
{$I ACBr.inc}

unit ACBrSATExtratoESCPOS;

interface

uses Classes, SysUtils,
     {$IFDEF FPC}
       LResources,
     {$ENDIF} 
     ACBrSATExtratoClass, ACBrPosPrinter,
     pcnCFe, pcnCFeCanc, pcnConversao;

type

   TACBrSATMarcaImpressora = (iEpson, iBematech);

  { TACBrSATExtratoESCPOS }
  TACBrSATExtratoESCPOS = class( TACBrSATExtratoClass )
  private
    FPosPrinter : TACBrPosPrinter ;

    procedure ImprimirCopias ;
    procedure SetPosPrinter(AValue: TACBrPosPrinter);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure GerarCabecalho;
    procedure GerarItens;
    procedure GerarTotais(Resumido : Boolean = False);
    procedure GerarPagamentos(Resumido : Boolean = False );
    procedure GerarObsFisco;
    procedure GerarDadosEntrega;
    procedure GerarObsContribuinte(Resumido : Boolean = False );
    procedure GerarRodape(CortaPapel: Boolean = True; Cancelamento: Boolean = False);
    procedure GerarDadosCancelamento;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirExtrato(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;
  published
    property PosPrinter : TACBrPosPrinter read FPosPrinter write SetPosPrinter;
  end ;

procedure Register;

implementation

uses ACBrValidador, ACBrUtil, ACBrDFeUtil;

{$IFNDEF FPC}
   {$R ACBrSATExtratoESCPOS.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrSAT',[TACBrSATExtratoESCPOS]);
end;

{ TACBrSATExtratoESCPOS }

constructor TACBrSATExtratoESCPOS.Create(AOwner: TComponent);
begin
  FPosPrinter := Nil;

  inherited create( AOwner );
end;

destructor TACBrSATExtratoESCPOS.Destroy;
begin
  inherited Destroy ;
end;

procedure TACBrSATExtratoESCPOS.GerarCabecalho;
begin
  FPosPrinter.ConfigBarras.Altura := 40;
  FPosPrinter.ConfigBarras.LarguraLinha := 2;
  FPosPrinter.ConfigBarras.MostrarCodigo := False;

  FPosPrinter.ConfigQRCode.LarguraModulo := 4;
  FPosPrinter.ConfigQRCode.ErrorLevel := 0;

  FPosPrinter.Buffer.Add('</zera></ce></logo>');
  FPosPrinter.Buffer.Add('<n>'+CFe.Emit.xFant+'</n>');

  FPosPrinter.Buffer.Add('<c>'+CFe.Emit.xNome);
  FPosPrinter.Buffer.Add(Trim(CFe.Emit.EnderEmit.xLgr)+' '+
                         Trim(CFe.Emit.EnderEmit.nro)+' '+
                         Trim(CFe.Emit.EnderEmit.xCpl)+' '+
                         Trim(CFe.Emit.EnderEmit.xBairro)+'-'+
                         Trim(CFe.Emit.EnderEmit.xMun)+'-'+
                         FormatarCEP(IntToStr(CFe.Emit.EnderEmit.CEP)));

  FPosPrinter.Buffer.Add( '</ae><c>'+
                          'CNPJ:'+FormatarCNPJ(CFe.Emit.CNPJCPF)+
                           ' IE:'+Trim(CFe.Emit.IE)+
                           ' IM:'+Trim(CFe.Emit.IM));
  FPosPrinter.Buffer.Add('</linha_simples>');


  if CFe.ide.tpAmb = taHomologacao then
  begin
    FPosPrinter.Buffer.Add('</fn></ce><n>Extrato No. 000000');
    FPosPrinter.Buffer.Add(ACBrStr('CUPOM FISCAL ELETRÔNICO - SAT</n>'));
    FPosPrinter.Buffer.Add(' ');
    FPosPrinter.Buffer.Add(' = T E S T E =');
    FPosPrinter.Buffer.Add(' ');
    FPosPrinter.Buffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
    FPosPrinter.Buffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
    FPosPrinter.Buffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
  end
  else
  begin
    FPosPrinter.Buffer.Add('</fn></ce><n>Extrato No. '+IntToStrZero(CFe.ide.nCFe,6));
    FPosPrinter.Buffer.Add( ACBrStr('CUPOM FISCAL ELETRÔNICO - SAT</n>'));
  end;

  FPosPrinter.Buffer.Add('</linha_simples>');
  FPosPrinter.Buffer.Add('</ae><c>CPF/CNPJ do Consumidor: '+
                         FormatarCNPJouCPF(CFe.Dest.CNPJCPF));
end;

procedure TACBrSATExtratoESCPOS.GerarItens;
var
  i : integer;
  LinhaCmd: String;
begin
  FPosPrinter.Buffer.Add('</fn></linha_simples>');
  FPosPrinter.Buffer.Add('#|COD|DESC|QTD|UN|VL UN R$|(VLTR R$)*|VL ITEM R$');
  FPosPrinter.Buffer.Add('</linha_simples>');

  for i:=0 to CFe.Det.Count - 1 do
  begin
    LinhaCmd := IntToStrZero(CFe.Det.Items[i].nItem,3)+' '+
                Trim(CFe.Det.Items[i].Prod.cProd)+' '+
                Trim(CFe.Det.Items[i].Prod.xProd)+' '+
                FormatFloatBr(CFe.Det.Items[i].Prod.qCom, Mask_qCom)+' '+
                Trim(CFe.Det.Items[i].Prod.uCom)+' X '+
                FormatFloatBr(CFe.Det.Items[i].Prod.vUnCom, Mask_vUnCom)+' ';

    if CFe.Det.Items[i].Imposto.vItem12741 > 0 then
      LinhaCmd := LinhaCmd + '('+FormatFloatBr(CFe.Det.Items[i].Imposto.vItem12741, '0.00')+') ';

    LinhaCmd := LinhaCmd + '|' + FormatFloatBr(CFe.Det.Items[i].Prod.vProd, '#,###,##0.00')+' ';

    LinhaCmd := PadSpace(LinhaCmd, FPosPrinter.ColunasFonteCondensada, '|');

    FPosPrinter.Buffer.Add('</ae><c>'+LinhaCmd);

    if CFe.Det.Items[i].Prod.vDesc > 0 then
    begin
      FPosPrinter.Buffer.Add(PadSpace('Desconto|'+
         FormatFloatBr(CFe.Det.Items[i].Prod.vDesc, '-#,###,##0.00'),
         FPosPrinter.ColunasFonteCondensada, '|'));
      FPosPrinter.Buffer.Add(ACBrStr(PadSpace('Valor líquido|'+
         FormatFloatBr(CFe.Det.Items[i].Prod.vProd - CFe.Det.Items[i].Prod.vDesc,
         '#,###,##0.00'),FPosPrinter.ColunasFonteCondensada, '|')));
    end;

    if CFe.Det.Items[i].Prod.vOutro > 0 then
    begin
      FPosPrinter.Buffer.Add(ACBrStr(PadSpace('Acréscimo|'+
         FormatFloatBr(CFe.Det.Items[i].Prod.vOutro, '+#,###,##0.00'),
         FPosPrinter.ColunasFonteCondensada, '|')));
      FPosPrinter.Buffer.Add(ACBrStr(PadSpace('Valor líquido|'+
         FormatFloatBr(CFe.Det.Items[i].Prod.vProd + CFe.Det.Items[i].Prod.vOutro,
         '#,###,##0.00'),FPosPrinter.ColunasFonteCondensada, '|')));
    end;

    if CFe.Det.Items[i].Imposto.ISSQN.vDeducISSQN > 0 then
    begin
      FPosPrinter.Buffer.Add(ACBrStr(PadSpace('Dedução para ISSQN|'+
         FormatFloatBr(CFe.Det.Items[i].Imposto.ISSQN.vDeducISSQN, '-#,###,##0.00'),
         FPosPrinter.ColunasFonteCondensada, '|')));
      FPosPrinter.Buffer.Add(ACBrStr(PadSpace('Base de cálculo ISSQN|'+
         FormatFloatBr(CFe.Det.Items[i].Imposto.ISSQN.vBC, '#,###,##0.00'),
         FPosPrinter.ColunasFonteCondensada, '|')));
    end;
  end;

  FPosPrinter.Buffer.Add('</ae></fn>');
end;

procedure TACBrSATExtratoESCPOS.GerarTotais(Resumido: Boolean);
var
  Descontos, Acrescimos: Double;
begin
  if not Resumido then
   begin
     Descontos  := (CFe.Total.ICMSTot.vDesc  + CFe.Total.DescAcrEntr.vDescSubtot);
     Acrescimos := (CFe.Total.ICMSTot.vOutro + CFe.Total.DescAcrEntr.vAcresSubtot);

     if (Descontos > 0) or (Acrescimos > 0) then
        FPosPrinter.Buffer.Add('<c>'+PadSpace('Subtotal|'+
           FormatFloatBr(CFe.Total.ICMSTot.vProd, '#,###,##0.00'),
           FPosPrinter.ColunasFonteCondensada, '|'));

     if CFe.Total.ICMSTot.vDesc > 0 then
        FPosPrinter.Buffer.Add('<c>'+PadSpace('Descontos|'+
           FormatFloatBr(Descontos, '-#,###,##0.00'),
           FPosPrinter.ColunasFonteCondensada, '|'));

     if CFe.Total.ICMSTot.vOutro > 0 then
        FPosPrinter.Buffer.Add('<c>'+ACBrStr(PadSpace('Acréscimos|'+
           FormatFloatBr(Acrescimos, '+#,###,##0.00'),
           FPosPrinter.ColunasFonteCondensada, '|')));
   end;

  FPosPrinter.Buffer.Add('</ae><e>'+PadSpace('TOTAL R$|'+
     FormatFloatBr(CFe.Total.vCFe, '#,###,##0.00'),
     trunc(FPosPrinter.ColunasFonteCondensada/2), '|')+'</e>');
end;

procedure TACBrSATExtratoESCPOS.GerarPagamentos(Resumido : Boolean = False );
var
  i : integer;
begin
  if not Resumido then
    FPosPrinter.Buffer.Add('');

  for i:=0 to CFe.Pagto.Count - 1 do
  begin
    FPosPrinter.Buffer.Add('<c>'+ACBrStr(PadSpace(CodigoMPToDescricao(CFe.Pagto.Items[i].cMP)+'|'+
                FormatFloatBr(CFe.Pagto.Items[i].vMP, '#,###,##0.00'),
                FPosPrinter.ColunasFonteCondensada, '|')));
  end;

  if CFe.Pagto.vTroco > 0 then
    FPosPrinter.Buffer.Add('<c>'+PadSpace('Troco R$|'+
       FormatFloatBr(CFe.Pagto.vTroco, '#,###,##0.00'),
       FPosPrinter.ColunasFonteCondensada, '|'));
end;

procedure TACBrSATExtratoESCPOS.GerarObsFisco;
var
  i : integer;
begin
  if (CFe.InfAdic.obsFisco.Count > 0) or
     (CFe.Emit.cRegTrib = RTSimplesNacional) then
     FPosPrinter.Buffer.Add('');

  if CFe.Emit.cRegTrib = RTSimplesNacional then
     FPosPrinter.Buffer.Add('<c>' + Msg_ICMS_123_2006 );

  for i:=0 to CFe.InfAdic.obsFisco.Count - 1 do
     FPosPrinter.Buffer.Add('<c>'+CFe.InfAdic.obsFisco.Items[i].xCampo+'-'+
                                  CFe.InfAdic.obsFisco.Items[i].xTexto);
end;

procedure TACBrSATExtratoESCPOS.GerarDadosEntrega;
begin
  if Trim(CFe.Entrega.xLgr)+
     Trim(CFe.Entrega.nro)+
     Trim(CFe.Entrega.xCpl)+
     Trim(CFe.Entrega.xBairro)+
     Trim(CFe.Entrega.xMun) <> '' then
   begin
     FPosPrinter.Buffer.Add('</fn></linha_simples>');
     FPosPrinter.Buffer.Add('DADOS PARA ENTREGA');
     FPosPrinter.Buffer.Add('<c>'+Trim(CFe.Entrega.xLgr)+' '+
                                  Trim(CFe.Entrega.nro)+' '+
                                  Trim(CFe.Entrega.xCpl)+' '+
                                  Trim(CFe.Entrega.xBairro)+' '+
                                  Trim(CFe.Entrega.xMun));
     FPosPrinter.Buffer.Add(CFe.Dest.xNome);
   end;
end;

procedure TACBrSATExtratoESCPOS.GerarObsContribuinte(Resumido : Boolean = False );
begin
  if Trim(CFe.InfAdic.infCpl) <> '' then
  begin
    FPosPrinter.Buffer.Add('</fn></linha_simples>');
    FPosPrinter.Buffer.Add(ACBrStr('OBSERVAÇÕES DO CONTRIBUINTE'));
    FPosPrinter.Buffer.Add('<c>'+StringReplace(Trim(CFe.InfAdic.infCpl),';',sLineBreak,[rfReplaceAll]));
  end;

  if CFe.Total.vCFeLei12741 > 0 then
  begin
    if Trim(CFe.InfAdic.infCpl) = '' then
    begin
      FPosPrinter.Buffer.Add('</fn></linha_simples>');
      FPosPrinter.Buffer.Add(ACBrStr('OBSERVAÇÕES DO CONTRIBUINTE'));
    end
    else
      FPosPrinter.Buffer.Add(' ');

    FPosPrinter.Buffer.Add('<c>'+PadSpace('Valor aproximado dos tributos do deste cupom R$ |<n>'+
                FormatFloatBr(CFe.Total.vCFeLei12741, '#,###,##0.00'),
                FPosPrinter.ColunasFonteCondensada, '|'));
    FPosPrinter.Buffer.Add('</n>(conforme Lei Fed. 12.741/2012)');

    if not Resumido then
    begin
      FPosPrinter.Buffer.Add(' ');
      FPosPrinter.Buffer.Add('*Valor aproximado dos tributos do item');
    end;
  end;
end;

procedure TACBrSATExtratoESCPOS.GerarRodape(CortaPapel: Boolean = True; Cancelamento: Boolean = False);
var
  QRCode: AnsiString;
begin
  FPosPrinter.Buffer.Add('</fn></linha_simples>');
  if Cancelamento then
     FPosPrinter.Buffer.Add(ACBrStr('<n>DADOS DO CUPOM FISCAL ELETRÔNICO CANCELADO</n>'));

  FPosPrinter.Buffer.Add('</ce>SAT No. <n>'+IntToStr(CFe.ide.nserieSAT)+'</n>');
  FPosPrinter.Buffer.Add(FormatDateTimeBr(CFe.ide.dEmi + CFe.ide.hEmi));
  FPosPrinter.Buffer.Add(' ');
  FPosPrinter.Buffer.Add('<c>'+FormatarChaveAcesso(CFe.infCFe.ID)+'</fn>');
  FPosPrinter.Buffer.Add(' ');

  FPosPrinter.Buffer.Add('<code128>'+copy(CFe.infCFe.ID,1,22)+'</code128>');
  FPosPrinter.Buffer.Add('<code128>'+copy(CFe.infCFe.ID,23,22)+'</code128>');
  FPosPrinter.Buffer.Add(' ');

  if ImprimeQRCode then
  begin
    QRCode := CalcularConteudoQRCode( CFe.infCFe.ID,
                                      CFe.ide.dEmi+CFe.ide.hEmi,
                                      CFe.Total.vCFe,
                                      Trim(CFe.Dest.CNPJCPF),
                                      CFe.ide.assinaturaQRCODE );

    FPosPrinter.Buffer.Add('<qrcode>'+QRCode+'</qrcode>');
  end;

  if CortaPapel then
    FPosPrinter.Buffer.Add('</corte_total>');
end;

procedure TACBrSATExtratoESCPOS.GerarDadosCancelamento;
var
  QRCode: AnsiString;
begin
  FPosPrinter.Buffer.Add('</fn></linha_simples>');
  FPosPrinter.Buffer.Add(ACBrStr('<n>DADOS DO CUPOM FISCAL ELETRÔNICO DE CANCELAMENTO</n>'));
  FPosPrinter.Buffer.Add('</ce>SAT No. <n>'+IntToStr(CFe.ide.nserieSAT)+'</n>');
  FPosPrinter.Buffer.Add(FormatDateTimeBr(CFeCanc.ide.dEmi + CFeCanc.ide.hEmi));
  FPosPrinter.Buffer.Add('');
  FPosPrinter.Buffer.Add('<c>'+FormatarChaveAcesso((CFeCanc.infCFe.ID))+'</fn>');
  FPosPrinter.Buffer.Add('');

  FPosPrinter.Buffer.Add('<code128>'+copy(CFeCanc.infCFe.ID,1,22)+'</code128>');
  FPosPrinter.Buffer.Add('<code128>'+copy(CFeCanc.infCFe.ID,23,22)+'</code128>');
  FPosPrinter.Buffer.Add(' ');

  if ImprimeQRCode then
  begin
    QRCode := CalcularConteudoQRCode( CFeCanc.infCFe.ID,
                                      CFeCanc.ide.dEmi+CFeCanc.ide.hEmi,
                                      CFeCanc.Total.vCFe,
                                      Trim(CFeCanc.Dest.CNPJCPF),
                                      CFeCanc.ide.assinaturaQRCODE );

    FPosPrinter.Buffer.Add('<qrcode>'+QRCode+'</qrcode>');
  end;

  FPosPrinter.Buffer.Add('</corte_total>');
end;

procedure TACBrSATExtratoESCPOS.ImprimirCopias;
begin
  FPosPrinter.Imprimir( '', True, True, NumCopias);   // Imprime o Buffer
end;

procedure TACBrSATExtratoESCPOS.SetPosPrinter(AValue: TACBrPosPrinter);
begin
  if AValue <> FPosPrinter then
  begin
     if Assigned(FPosPrinter) then
        FPosPrinter.RemoveFreeNotification(Self);

     FPosPrinter := AValue;

     if AValue <> nil then
        AValue.FreeNotification(self);
  end ;
end;

procedure TACBrSATExtratoESCPOS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TACBrPosPrinter) and (FPosPrinter <> nil) then
       FPosPrinter := nil ;
  end;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtrato(ACFe: TCFe);
begin
  inherited;

  GerarCabecalho;
  GerarItens;
  GerarTotais;
  GerarPagamentos;
  GerarObsFisco;
  GerarDadosEntrega;
  GerarObsContribuinte;
  GerarRodape;

  ImprimirCopias;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  inherited;

  GerarCabecalho;
  GerarTotais(True);
  GerarRodape(False, True);
  GerarDadosCancelamento;

  ImprimirCopias;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtratoResumido(ACFe: TCFe);
begin
  inherited;

  GerarCabecalho;
  GerarTotais(True);
  GerarPagamentos(True);
  GerarObsFisco;
  GerarDadosEntrega;
  GerarObsContribuinte(True);
  GerarRodape;

  ImprimirCopias;
end;

{$IFDEF FPC}
{$IFNDEF NOGUI}
initialization
   {$I ACBrSATExtratoESCPOS.lrs}
{$ENDIF}
{$ENDIF}

end.
