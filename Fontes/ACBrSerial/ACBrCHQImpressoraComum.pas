{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 30/08/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrCHQImpressoraComum
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQImpressoraComum;

interface
uses ACBrCHQClass,  
     Classes ;

const cColCheque = 65 ;
      cLinCheque = 18 ;
      cCmdImpCondensado = #15 ;
      cCmdImpZera = #27+'@' ;
      cCmdImp_7_72 = #27+'1' ;
      cCmdTamanhoPag = #27+'C';{ Acrescentar chr(Tamanho); Cheque usa 18 linhas}
      cAF = 1.18644 ;          { Constante de conversão da metrica da
                                 espaçamento horizontal de caracteres
                                 usada pela Bematech para uma Matricial comum }

const MesDescr : array[1..12] of string =
      ('Janeiro','Fevereiro','Março','Abril','Maio','Junho','Julho','Agosto',
       'Setembro','Outubro','Novembro','Dezembro') ;

type TACBrCHQImpressoraComum = class( TACBrCHQClass )
  private
    fsLinhaInicial : Integer;
    fsColunaInicial: Integer;
    fsColCheque: Integer;
    fsCmdImpCondensado: String;
    fsCmdImpZera: String;
    fsCmdImp_7_72: String;
    fsCmdTamanhoPag: String;
    fsLinCheque: Integer;

  protected
    function GetChequePronto: Boolean; Override ;

  public
    property CmdImpZera  : String read fsCmdImpZera write fsCmdImpZera ;
    property CmdImp_7_72 : String read fsCmdImp_7_72 write fsCmdImp_7_72 ;
    property CmdTamanhoPag    : String read fsCmdTamanhoPag
       write fsCmdTamanhoPag ;
    property CmdImpCondensado : String read fsCmdImpCondensado
       write fsCmdImpCondensado ;
    property ColCheque     : Integer read fsColCheque     write fsColCheque;
    property LinCheque     : Integer read fsLinCheque     write fsLinCheque;
    Property LinhaInicial  : Integer read fsLinhaInicial  write fsLinhaInicial ;
    Property ColunaInicial : Integer read fsColunaInicial write fsColunaInicial;

    constructor Create(AOwner: TComponent);

    procedure Ativar ; override ;

    procedure ImprimirCheque ; Override ;
end ;

implementation
Uses ACBrUtil, ACBrExtenso,
     SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5{$ENDIF} ;

{ TACBrCHQImpressoraComum }

constructor TACBrCHQImpressoraComum.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fsCmdImpZera      := cCmdImpZera ;
  fsCmdImp_7_72     := cCmdImp_7_72 ;
  fsCmdTamanhoPag   := cCmdTamanhoPag ;
  fsCmdImpCondensado:= cCmdImpCondensado ;
  fsColCheque       := cColCheque ;
  fsLinCheque       := cLinCheque ;
  fsLinhaInicial    := 0 ;
  fsColunaInicial   := 0 ;
  fpDevice.HardFlow := true ;
  fpModeloStr       := 'Imp.Comum' ;
end;

procedure TACBrCHQImpressoraComum.Ativar;
begin
  if fpDevice.Porta = '' then
     raise Exception.Create(ACBrStr('Impressora de Cheques '+fpModeloStr+' requer'+#10+
                            'Porta Serial (COMn) ou Paralela (LPTn)'));

  inherited Ativar ; { Apenas ajusta fpAtivo }
end;

function TACBrCHQImpressoraComum.GetChequePronto: Boolean;
begin
  Result := fpDevice.EmLinha ;
end;

procedure TACBrCHQImpressoraComum.ImprimirCheque;
Var Modelo : TACBrCHQModelo ;
    Linhas : TStringList ;
    ACBrEXT: TACBrExtenso ;
    Ext1,Ext2,Tracos,Texto : String ;
    Pos,nCharExt1,nCharExt2,nCharLocal,nCharValor : Integer ;
begin
  if not fpDevice.EmLinha( 3 ) then  { Impressora está em-linha ? }
    raise Exception.Create(ACBrStr('A impressora de Cheques '+fpModeloStr+
                           ' não está pronta.')) ;

  Modelo := fpModelosCheque.AchaModeloBanco(fpBanco) ;

  if Modelo = nil then
     raise Exception.Create(ACBrStr('Modelo de Cheque do Banco:'+fpBanco+
                            'não encontrado.')) ;

  { Progamando a impressora para Formulário de Tamanho do Cheque }
  fpDevice.EnviaString( CmdImpZera );
  fpDevice.EnviaString( CmdTamanhoPag + chr(fsLinCheque) ) ;  
  fpDevice.EnviaString( CmdImp_7_72 ) ;

  { Calculando o Extenso }
  Texto   := '' ;
  Tracos  := '' ;
  ACBrEXT := TACBrExtenso.Create(nil);
  try
     ACBrEXT.Valor := fpValor ;

     // Utilizar pagina de codigo para substituir acentos
     Texto := CodificarPaginaDeCodigo(ACBrEXT.Texto);
  finally
     ACBrEXT.Free ;
  end ;

  { Verificando se o extenso cabe na linha de cima }
  nCharExt1 := ColCheque - Round( Modelo.ColunaExtenso1 / cAF ) ;
  nCharExt2 := ColCheque - Round( Modelo.ColunaExtenso2 / cAF ) ;
  Tracos:= '' ;
  Texto := '( '+Trim(Texto)+' )' ;
  Pos   := Length( Texto );

  while (Pos > nCharExt1) do
  begin
     { Acha um espaço }
     while (Texto[Pos] <> ' ') and (Pos > 0) do
        Pos := Pos - 1 ;

     Pos := Pos - 1 ;
  end ;

  { Inserindo traços no inicio }
  if Pos < Length( Texto ) then
     while (Pos + Length(Tracos)) < nCharExt1 do
        Tracos  := Tracos + '-' ;

  Ext1  := PadRight(Tracos + copy(Texto,1,Pos), nCharExt1,'*') ;
  Ext2  := PadRight(copy(Texto,Pos+2, Length(Texto) ), nCharExt2,'*') ; ;

  { Ordenando as Linhas de acordo com Linha/Coluna }
  Linhas  := TStringList.Create ;
  try
     with Modelo do
     begin
        nCharLocal := Round( (ColunaDia   - ColunaLocal - 2) / cAF ) ;
        nCharValor := fsColCheque - Round( ColunaValor / cAF ) - 1   ;

        Linhas.Add(IntToStrZero(LinhaValor,3)+'|'+
             IntToStrZero(Round(ColunaValor/cAF),3)+'|'+PadLeft('('+
             Trim(FormatFloat('###,###,##0.00',fpValor))+')',nCharValor) ) ;
        Linhas.Add(IntToStrZero(LinhaExtenso1,3)+'|'+
                   IntToStrZero(Round(ColunaExtenso1/cAF),3)+'|'+ Ext1 ) ;
        Linhas.Add(IntToStrZero(LinhaExtenso2,3)+'|'+
                   IntToStrZero(Round(ColunaExtenso2/cAF),3)+'|'+ Ext2 ) ;
        Linhas.Add(IntToStrZero(LinhaFavorecido,3)+'|'+
                   IntToStrZero(Round(ColunaFavorecido/cAF),3)+'|( '+
                   fpFavorecido+' )' ) ;

        Linhas.Add(IntToStrZero(LinhaLocal,3)+'|'+
                   IntToStrZero(Round(ColunaLocal/cAF),3)+
                   '|'+PadLeft(fpCidade, nCharLocal) ) ;
        Linhas.Add(IntToStrZero(LinhaLocal,3)+'|'+
                   IntToStrZero(Round(ColunaDia/cAF),3)+'|'+
                   IntToStr( DayOf(fpData) ) ) ;
        Linhas.Add(IntToStrZero(LinhaLocal,3)+'|'+
                   IntToStrZero(Round(ColunaMes/cAF),3)+'|'+
                   CodificarPaginaDeCodigo( ACBrStr(MesDescr[ MonthOf(fpData) ]) ) ) ;

        Linhas.Add(IntToStrZero(LinhaLocal,3)+'|'+
                   IntToStrZero(Round(ColunaAno/cAF),3)+'|'+
                   IntToStr(YearOf(fpData))) ;
        Linhas.Sort ;

        { Imprimindo }
        For Pos := 0 to Linhas.Count - 1 do
           fpDevice.ImprimePos(StrToInt(copy(Linhas[Pos],1,3)),
                               StrToInt(copy(Linhas[Pos],5,3)),
                               copy(Linhas[Pos],9,Length(Linhas[Pos])) ) ;
     end ;
  finally
     Linhas.Free ;
  end ;

  fpDevice.Eject ;
end;

end.

