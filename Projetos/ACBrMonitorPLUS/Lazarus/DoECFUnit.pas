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

unit DoECFUnit ;

interface
Uses Classes, TypInfo, SysUtils, CmdUnit ;

Procedure DoECF( Cmd : TACBrCmd ) ;
Function PegaAliquotas : String ;
Function PegaRelatoriosGerenciais : String ;
Function PegaTotaisRelatoriosGerenciais : String ;
Function PegaTotaisAliquotas : String ;
Function PegaFormasPagamento : String ;
Function PegaTotaisFormasPagamento : String ;
Function PegaComprovantesNaoFiscais : String ;
Function PegaTotaisComprovantesNaoFiscais : String ;
Function PegaUnidadesMedida : String ;

Procedure StringToMemo( AString : AnsiString; Memo : TStringList );

implementation
uses StrUtils, ACBrECF, ACBrDevice, ACBrECFClass, ACBrUtil, UtilUnit,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

function AjustaNomeArquivoCmd( Cmd : TACBrCmd; Param: Integer = 2 ) : String ;
begin
  if Cmd.Params(Param) <> '' then
     Result := Cmd.Params(Param)
  else
     Result := Cmd.Metodo+'.txt' ;

  Result := AcertaPath( Result );
end;

Procedure DoECF( Cmd : TACBrCmd ) ;
Var wDescricao  : AnsiString ;
    Linhas      : TStringList ;
    Linha       : AnsiString ;
    NomeArquivo : String ;
    FPG         : TACBrECFFormaPagamento ;
    REL         : TACBrECFRelatorioGerencial;
    ICMS        : TACBrECFAliquota ;
    CNF         : TACBrECFComprovanteNaoFiscal ;
    Finalidade  : TACBrECFFinalizaArqMFD;
    TipoDoc     : TACBrECFTipoDocumento;
    TipoDocStr, FinalidadeStr: String;
    FXMLOriginal : TStringList;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     try
        if Cmd.Metodo = 'achar' then
        begin
          Cmd.Resposta := BoolToStr(AcharECF(StrToBoolDef(Trim(Cmd.Params(0)),true),
            StrToBoolDef(Trim(Cmd.Params(1)),true), StrToIntDef(Trim(Cmd.Params(1)), 3)), true);
        end
        else
        if Cmd.Metodo = 'ativar' then  { Ativa o ecf }
         begin
           Ativar ;
           {$IFNDEF NOGUI}FrmACBrMonitor.AvaliaEstadoTsECF ;{$ENDIF}
         end

        else if Cmd.Metodo = 'desativar' then
         begin
           Desativar ;
           {$IFNDEF NOGUI}FrmACBrMonitor.AvaliaEstadoTsECF ;{$ENDIF}
         end

        else if Cmd.Metodo = 'ativo' then
           Cmd.Resposta := BoolToStr(Ativo, true)

        else if Cmd.Metodo = 'colunas' then
           Cmd.Resposta := IntToStr( Colunas )

        else if Cmd.Metodo = 'paramdescontoissqn' then
              Cmd.Resposta:= BoolToStr(ParamDescontoISSQN, True)

        else if (Cmd.Metodo = 'enviainfo') or (Cmd.Metodo = 'retornainfoecf') then
            Cmd.Resposta :=   RetornaInfoECF(Cmd.Params(0))

        else if Cmd.Metodo = 'comandoenviado' then
           Cmd.Resposta := ComandoEnviado

        else if Cmd.Metodo = 'respostacomando' then
           Cmd.Resposta := RespostaComando

        else if Cmd.Metodo = 'modelostr' then
           Cmd.Resposta := ModeloStr

        else if Cmd.Metodo = 'modelo' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrECFModelo),Integer(Modelo))

        else if Cmd.Metodo = 'porta' then
           Cmd.Resposta := Porta

        else if Cmd.Metodo = 'timeout' then
           Cmd.Resposta := IntToStr( TimeOut )

        else if Cmd.Metodo = 'settimeout' then
           TimeOut := StrToInt( Trim(Cmd.Params(0)) )

        else if Cmd.Metodo = 'intervaloaposcomando' then
           Cmd.Resposta := IntToStr( IntervaloAposComando )

        else if Cmd.Metodo = 'descricaogrande' then
           Cmd.Resposta := BoolToStr( DescricaoGrande, true )

        else if Cmd.Metodo = 'gavetasinalinvertido' then
           Cmd.Resposta := BoolToStr( GavetaSinalInvertido, true )

        else if Cmd.Metodo = 'ignorartagsformatacao' then
           Cmd.Resposta := BoolToStr( IgnorarTagsFormatacao, true )

        else if Cmd.Metodo = 'controleporta' then
           Cmd.Resposta := BoolToStr( ControlePorta, true )

        else if Cmd.Metodo = 'operador' then
           Cmd.Resposta := Operador

        else if Cmd.Metodo = 'msgaguarde' then
           Cmd.Resposta := MsgAguarde

        else if Cmd.Metodo = 'msgtrabalhando' then
           Cmd.Resposta := MsgTrabalhando

        else if Cmd.Metodo = 'msgpoucopapel' then
           Cmd.Resposta := IntToStr( MsgPoucoPapel )

        else if Cmd.Metodo = 'exibemensagem' then
           Cmd.Resposta := BoolToStr( ExibeMensagem, true )

        else if Cmd.Metodo = 'bloqueiamouseteclado' then
           Cmd.Resposta := BoolToStr( BloqueiaMouseTeclado, true )

        else if Cmd.Metodo = 'linhasentrecupons' then
           Cmd.Resposta := IntToStr( LinhasEntreCupons )

        else if Cmd.Metodo = 'paginadecodigo' then
           Cmd.Resposta := IntToStr( PaginaDeCodigo )

        else if Cmd.Metodo = 'maxlinhasbuffer' then
           Cmd.Resposta := IntToStr( MaxLinhasBuffer )

        else if Cmd.Metodo = 'setmaxlinhasbuffer' then
           MaxLinhasBuffer := StrToInt( Trim(Cmd.Params(0)) )

        else if Cmd.Metodo = 'datahora' then
           Cmd.Resposta := FormatDateTime('dd/mm/yy hh:nn:ss', DataHora )

        else if (Cmd.Metodo = 'numcupom') or (Cmd.Metodo = 'numcoo') then
         begin
           Cmd.Resposta := NumCupom ;
           if Cmd.Resposta = '' then
              raise Exception.Create('Erro na leitura do COO');
         end

        else if Cmd.Metodo = 'numloja' then
           Cmd.Resposta := NumLoja

        else if Cmd.Metodo = 'numcro' then
           Cmd.Resposta := NumCRO

        else if Cmd.Metodo = 'numccf' then
           Cmd.Resposta := NumCCF

        else if Cmd.Metodo = 'numgrg' then
           Cmd.Resposta := NumGRG

        else if Cmd.Metodo = 'numgnf' then
           Cmd.Resposta := NumGNF

        else if Cmd.Metodo = 'numgnfc' then
           Cmd.Resposta := NumGNFC

        else if Cmd.Metodo = 'numcdc' then
           Cmd.Resposta := NumCDC

        else if Cmd.Metodo = 'numcfc' then
           Cmd.Resposta := NumCFC

        else if Cmd.Metodo = 'numccdc' then
           Cmd.Resposta := NumCCDC

        else if Cmd.Metodo = 'numcfd' then
           Cmd.Resposta := NumCFD

        else if Cmd.Metodo = 'numncn' then
           Cmd.Resposta := NumNCN

        else if Cmd.Metodo = 'numcrz' then
           Cmd.Resposta := NumCRZ

        else if Cmd.Metodo = 'numecf' then
           Cmd.Resposta := NumECF

        else if Cmd.Metodo = 'numserie' then
           Cmd.Resposta := NumSerie

        else if Cmd.Metodo = 'numseriemfd' then
           Cmd.Resposta := NumSerieMFD

        else if Cmd.Metodo = 'numversao' then
           Cmd.Resposta := NumVersao

        else if Cmd.Metodo = 'mfadicional' then
           Cmd.Resposta := MFAdicional

        else if Cmd.Metodo = 'rfdid' then
           Cmd.Resposta := RFDID

        else if Cmd.Metodo = 'datamovimento' then
           Cmd.Resposta := FormatDateTime('dd/mm/yy', DataMovimento )

        else if Cmd.Metodo = 'cnpj' then
           Cmd.Resposta := CNPJ

        else if Cmd.Metodo = 'ie' then
           Cmd.Resposta := IE

        else if Cmd.Metodo = 'im' then
           Cmd.Resposta := IM

        else if Cmd.Metodo = 'cliche' then
           Cmd.Resposta := Cliche

        else if Cmd.Metodo = 'usuarioatual' then
           Cmd.Resposta := UsuarioAtual

        else if Cmd.Metodo = 'datahorasb' then
           Cmd.Resposta := FormatDateTime('dd/mm/yy hh:nn:ss', DataHoraSB )

        else if Cmd.Metodo = 'datahoraultimareducaoz' then
           Cmd.Resposta := FormatDateTime('dd/mm/yy hh:nn:ss', DataHoraUltimaReducaoZ )

        else if Cmd.Metodo = 'decimaisqtd' then
           Cmd.Resposta := IntToStr( DecimaisQtd )

        else if Cmd.Metodo = 'decimaispreco' then
           Cmd.Resposta := IntToStr( DecimaisPreco )

        else if Cmd.Metodo = 'submodeloecf' then
           Cmd.Resposta := SubModeloECF

        else if Cmd.Metodo = 'paf' then
           Cmd.Resposta := PAF

        else if Cmd.Metodo = 'numcooinicial' then
           Cmd.Resposta := NumCOOInicial

        else if Cmd.Metodo = 'vendabruta' then
           Cmd.Resposta := FloatToStr( VendaBruta )

        else if Cmd.Metodo = 'grandetotal' then
           Cmd.Resposta := FloatToStr( GrandeTotal )

        else if Cmd.Metodo = 'totaltroco' then
           Cmd.Resposta := FloatToStr( TotalTroco )

        else if Cmd.Metodo = 'totalcancelamentos' then
           Cmd.Resposta := FloatToStr( TotalCancelamentos )

        else if Cmd.Metodo = 'totaldescontos' then
           Cmd.Resposta := FloatToStr( TotalDescontos )

        else if Cmd.Metodo = 'totalacrescimos' then
           Cmd.Resposta := FloatToStr( TotalAcrescimos )

        else if Cmd.Metodo = 'totalsubstituicaotributaria' then
           Cmd.Resposta := FloatToStr( TotalSubstituicaoTributaria )

        else if Cmd.Metodo = 'totalnaotributado' then
           Cmd.Resposta := FloatToStr( TotalNaoTributado )

        else if Cmd.Metodo = 'totalisencao' then
           Cmd.Resposta := FloatToStr( TotalIsencao )

        else if Cmd.Metodo = 'totalcancelamentosissqn' then
           Cmd.Resposta := FloatToStr( TotalCancelamentosISSQN )

        else if Cmd.Metodo = 'totaldescontosissqn' then
           Cmd.Resposta := FloatToStr( TotalDescontosISSQN )

        else if Cmd.Metodo = 'totalacrescimosissqn' then
           Cmd.Resposta := FloatToStr( TotalAcrescimosISSQN )

        else if Cmd.Metodo = 'totalsubstituicaotributariaissqn' then
           Cmd.Resposta := FloatToStr( TotalSubstituicaoTributariaISSQN )

        else if Cmd.Metodo = 'totalnaotributadoissqn' then
           Cmd.Resposta := FloatToStr( TotalNaoTributadoISSQN )

        else if Cmd.Metodo = 'totalisencaoissqn' then
           Cmd.Resposta := FloatToStr( TotalIsencaoISSQN )

        else if Cmd.Metodo = 'totalnaofiscal' then
           Cmd.Resposta := FloatToStr( TotalNaoFiscal )

        else if Cmd.Metodo = 'totalcancelamentosopnf' then
           Cmd.Resposta := FloatToStr( TotalCancelamentosOPNF )

        else if Cmd.Metodo = 'totaldescontosopnf' then
           Cmd.Resposta := FloatToStr( TotalDescontosOPNF )

        else if Cmd.Metodo = 'totalacrescimosopnf' then
           Cmd.Resposta := FloatToStr( TotalAcrescimosOPNF )

        else if Cmd.Metodo = 'numultitem' then
           Cmd.Resposta := IntToStr( NumUltItem )

        else if Cmd.Metodo = 'dadosreducaoz' then
           Cmd.Resposta := DadosReducaoZ 

        else if Cmd.Metodo = 'dadosultimareducaoz' then
           Cmd.Resposta := DadosUltimaReducaoZ

        else if Cmd.Metodo = 'numreducoeszrestantes' then
           Cmd.Resposta := NumReducoesZRestantes

        else if Cmd.Metodo = 'aliquotas' then
           Cmd.Resposta := PegaAliquotas

        else if Cmd.Metodo = 'carregaaliquotas' then
         begin
           CarregaAliquotas ;
           Cmd.Resposta := PegaAliquotas ;
         end

        else if Cmd.Metodo = 'lertotaisaliquota' then
           Cmd.Resposta := PegaTotaisAliquotas
           
        else if Cmd.Metodo = 'programaaliquota' then
            ProgramaAliquota( StringToFloat( Cmd.Params(0) ), {% Aliquota }
                              PadLeft(Cmd.Params(1),1,'T')[1],  { Tipo: char(T, S)}
                              Cmd.Params(2) )                   { Posicao = '' }

        else if Cmd.Metodo = 'achaicmsaliquota' then
         begin
           ICMS := AchaICMSAliquota( StringToFloat( Cmd.Params(0) ),{% Aliquota}
                                     PadLeft(Cmd.Params(1),1)[1]) ;  { Tipo = ' ' }
           if ICMS <> nil then
              Cmd.Resposta := PadLeft(ICMS.Indice,4) +
                              ICMS.Tipo +
                              FormatFloat('##0.00', ICMS.Aliquota )
           else
              raise Exception.Create('Aliquota: '+
                                     Trim(cmd.Params(0)+' '+cmd.Params(1))+
                                     ' não encontrada');
         end

        else if Cmd.Metodo = 'formaspagamento' then
           Cmd.Resposta := PegaFormasPagamento

        else if Cmd.Metodo = 'carregaformaspagamento' then
         begin
           CarregaFormasPagamento ;
           Cmd.Resposta := PegaFormasPagamento
         end

        else if Cmd.Metodo = 'lertotaisformapagamento' then
           Cmd.Resposta := PegaTotaisFormasPagamento

        else if Cmd.Metodo = 'programaformapagamento' then
         begin
            wDescricao := Cmd.Params(0) ;
            ProgramaFormaPagamento( wDescricao ,                   { Descricao }
                   StrToBoolDef(Trim(Cmd.Params(1)),true),    {Permitevinculado}
                   Cmd.Params(2) ) ;                            { Posicao = '' }
         end

        else if Cmd.Metodo = 'achafpgdescricao' then
         begin
           FPG := AchaFPGDescricao( cmd.Params(0),                 { Descricao }
                     StrToBoolDef(Trim(Cmd.Params(1)),False) ) ;  { BuscaExata }
           if FPG <> nil then
              Cmd.Resposta := PadLeft( FPG.Indice,4)+
                              IfThen(FPG.PermiteVinculado,'V',' ')+
                              PadLeft( FPG.Descricao, 30)
           else
              raise Exception.Create('Forma de Pagamento: '+Trim(cmd.Params(0))+
                                     ' não encontrada');
         end

        else if Cmd.Metodo = 'comprovantesnaofiscais' then
           Cmd.Resposta := PegaComprovantesNaoFiscais

        else if Cmd.Metodo = 'carregacomprovantesnaofiscais' then
         begin
           CarregaComprovantesNaoFiscais ;
           Cmd.Resposta := PegaComprovantesNaoFiscais
         end

        else if Cmd.Metodo = 'lertotaiscomprovantenaofiscal' then
           Cmd.Resposta := PegaTotaisComprovantesNaoFiscais

        else if Cmd.Metodo = 'programacomprovantenaofiscal' then
         begin
            wDescricao := Cmd.Params(0) ;
            ProgramaComprovanteNaoFiscal( wDescricao,           { Descricao }
                                          Cmd.Params(1),        { Tipo = '' }
                                          Cmd.Params(2) );      { Posicao = '' }
         end

        else if Cmd.Metodo = 'achacnfdescricao' then
         begin
           CNF := AchaCNFDescricao( cmd.Params(0),                 { Descricao }
                     StrToBoolDef(Trim(Cmd.Params(1)),False) ) ;  { BuscaExata } 
           if CNF <> nil then
              Cmd.Resposta := PadLeft( CNF.Indice,4) +
                              IfThen(CNF.PermiteVinculado,'V',' ')+
                              PadLeft( CNF.Descricao,30) +
                              PadLeft( CNF.FormaPagamento,4) 
           else
              raise Exception.Create('Comprovante Não Fiscal: '+Trim(cmd.Params(0))+
                                     ' não encontrado');
         end

        else if Cmd.Metodo = 'unidadesmedida' then
           Cmd.Resposta := PegaUnidadesMedida

        else if Cmd.Metodo = 'carregaunidadesmedida' then
         begin
           CarregaUnidadesMedida ;
           Cmd.Resposta := PegaUnidadesMedida ;
         end

        else if Cmd.Metodo = 'programaunidademedida' then
         begin
            wDescricao := Cmd.Params(0) ;
            ProgramaUnidadeMedida( wDescricao ) ;
         end

        else if Cmd.Metodo = 'relatoriosgerenciais' then
           Cmd.Resposta := PegaRelatoriosGerenciais

        else if Cmd.Metodo = 'carregarelatoriosgerenciais' then
         begin
           CarregaRelatoriosGerenciais ;
           Cmd.Resposta := PegaRelatoriosGerenciais;
         end

        else if Cmd.Metodo = 'lertotaisrelatoriosgerenciais' then
           Cmd.Resposta := PegaTotaisRelatoriosGerenciais

        else if Cmd.Metodo = 'programarelatoriosgerenciais' then
         begin
            wDescricao := Cmd.Params(0) ;
            ProgramaRelatoriosGerenciais( wDescricao ,             { Descricao }
                   Cmd.Params(1) ) ;                            { Posicao = '' }
         end

        else if Cmd.Metodo = 'achargdescricao' then
         begin
           REL := AchaRGDescricao( cmd.Params(0),                 { Descricao }
                     StrToBoolDef(Trim(Cmd.Params(1)),False) ) ;  { BuscaExata }
           if REL <> nil then
              Cmd.Resposta := PadLeft(REL.Indice,4) +
                              PadLeft( REL.Descricao, 30) +
                              IntToStrZero( REL.Contador, 5 )
           else
              raise Exception.Create('Relatório Gerencial: '+Trim(cmd.Params(0))+
                                     ' não encontrado');
         end

        else if Cmd.Metodo = 'testapodeabrircupom' then
           TestaPodeAbrirCupom

        else if Cmd.Metodo = 'identificaoperador' then
           IdentificaOperador( Cmd.Params(0) )                          { Nome }

        else if Cmd.Metodo = 'identificaconsumidor' then
           IdentificaConsumidor( Cmd.Params(0),                   { CPF / CNPJ }
                                 Cmd.Params(1),                         { NOME }
                                 Cmd.Params(2) )                    { ENDERECO }

        else if Cmd.Metodo = 'identificapaf' then
           IdentificaPAF( Cmd.Params(0),                             { Linha 1 }
                          Cmd.Params(1) )                            { Linha 2 }

        else if Cmd.Metodo = 'abrecupom' then
           AbreCupom( Cmd.Params(0),                              { CPF / CNPJ }
                      Cmd.Params(1),                                    { NOME }
                      Cmd.Params(2) )                               { ENDERECO }

        else if Cmd.Metodo = 'legendainmetroproximoitem' then
           LegendaInmetroProximoItem

        else if Cmd.Metodo = 'vendeitem' then
         begin
           VendeItem( Cmd.Params(0), Cmd.Params(1),           { Cod, Descricao }
                      Trim(Cmd.Params(2)),                          { Aliquota }
                      StringToFloat( Cmd.Params(3)),                     { Qtd }
                      StringToFloat( Cmd.Params(4)),                  { P.Unit }
                      StringToFloatDef( Cmd.Params(5), 0),{ Val.Desconto/Acres }
                      Cmd.Params(6),                                      { Un }
                      PadLeft(Cmd.Params(7),1,'%'),               { Tipo Desconto }
                      PadLeft(Cmd.Params(8),1,'D') );             { Desc / Acresc }
          { Aguarda 1 segundo ou até o ECF ficar Em linha novamente }
           EmLinha( 1 ) ;
          { O comando acima é util para evitar erros na Impressao de Itens NAO
            concomitante, (imprimir todo o cupom), Pois o método "VendeItem" NAO
            seta "AguardaImpressao := True" para ficar mais rápido }
         end

        else if Cmd.Metodo = 'descontoacrescimoitemanterior' then
           DescontoAcrescimoItemAnterior(
                 StringToFloat( Cmd.Params(0)),                        { Valor }
                 PadLeft(Cmd.Params(1),1,'D'),                       { 'D' ou 'A' }
                 PadLeft(Cmd.Params(2),1,'%'),                       { '%' ou '$' }
                 StrToIntDef(Cmd.Params(3),0) )                  { NumItem = 0 }

        else if Cmd.Metodo = 'subtotalizacupom' then
           SubtotalizaCupom( StringToFloatDef( Cmd.Params(0), 0),  {Acresc/Desc}
                             Cmd.Params(1) )                  { Msg.Rodape = ''}

        else if Cmd.Metodo = 'efetuapagamento' then
           EfetuaPagamento( Cmd.Params(0),                 { CodFormaPagamento }
                            StringToFloat( Cmd.Params(1)),             { Valor }
                            Cmd.Params(2),                        { Observacao }
                            StrToBoolDef(Trim(Cmd.Params(3)),false) ){ Imp.Vinculado }

        else if Cmd.Metodo = 'estornapagamento' then
                   EstornaPagamento( Cmd.Params(0),                 { CodFormaPagamentoEstornar }
                                     Cmd.Params(1),                 { CodFormaPagamentoEfetivar }
                                     StringToFloat( Cmd.Params(2)),    { Valor }
                                     Cmd.Params(3) )                { Observacao }

        else if Cmd.Metodo = 'fechacupom' then
           FechaCupom( Cmd.Params(0) )

        else if Cmd.Metodo = 'cancelacupom' then
           CancelaCupom

        else if Cmd.Metodo = 'cancelaitemvendido' then
           CancelaItemVendido( StrToInt(Trim(Cmd.Params(0)) ) )           { NumItem }

        else if Cmd.Metodo = 'cancelaitemvendidoparcial' then
           CancelaItemVendidoParcial( StrToInt(Trim(Cmd.Params(0)) ),     { NumItem }
                                      StringToFloat(Cmd.Params(1)) )      { Valor }

        else if Cmd.Metodo = 'canceladescontoacrescimoitem' then
           CancelaDescontoAcrescimoItem( StrToInt(Trim(Cmd.Params(0)) ) ) { NumItem }

        else if Cmd.Metodo = 'canceladescontoacrescimosubtotal' then
           CancelaDescontoAcrescimoSubTotal(PadLeft(Trim(Cmd.Params(0)),1,'D')[1]){ Tipo : D - Desconto, A - Acréscimo }

        else if Cmd.Metodo = 'subtotal' then
           Cmd.Resposta := FloatToStr( Subtotal )

        else if Cmd.Metodo = 'totalpago' then
           Cmd.Resposta := FloatToStr( TotalPago )

        else if Cmd.Metodo = 'sangria' then
           Sangria( StringToFloat( Cmd.Params(0)),                     { Valor }
                    Cmd.Params(1),                                       { Obs }
                    Cmd.Params(2),                              { DescricaoCNF }
                    Cmd.Params(3))                              { DescricaoFPG }

        else if Cmd.Metodo = 'suprimento' then
           Suprimento( StringToFloat( Cmd.Params(0)),                  { Valor }
                       Cmd.Params(1),                                    { Obs }
                       Cmd.Params(2),                           { DescricaoCNF }
                       Cmd.Params(3))                           { DescricaoFPG }

        else if Cmd.Metodo = 'cortapapel' then
           CortaPapel( StrToBoolDef(Trim(Cmd.Params(0)),false) ) { Corte Parcial }

        else if Cmd.Metodo = 'naofiscalcompleto' then
           NaoFiscalCompleto( Cmd.Params(0),                          { CodCNF }
                              StringToFloat(Cmd.Params(1)),            { Valor }
                              Cmd.Params(2),                   { CodFormaPagto }
                              Cmd.Params(3) )                            { Obs }

        else if Cmd.Metodo = 'abrenaofiscal' then
           AbreNaoFiscal( Cmd.Params(0) )                           { CPF_CNPJ }

        else if Cmd.Metodo = 'registraitemnaofiscal' then
           RegistraItemNaoFiscal( Cmd.Params(0),                      { CodCNF }
                              StringToFloat(Cmd.Params(1)),            { Valor }
                              Cmd.Params(2) )                            { Obs }

        else if Cmd.Metodo = 'cancelaitemnaofiscal' then
           CancelaItemNaoFiscal( StrToInt(Trim(Cmd.Params(0)) ) )       { NumItem }

        else if Cmd.Metodo = 'subtotalizanaofiscal' then
           SubtotalizaNaoFiscal( StringToFloatDef( Cmd.Params(0), 0) )  {Acresc/Desc}

        else if Cmd.Metodo = 'efetuapagamentonaofiscal' then
           EfetuaPagamentoNaoFiscal( Cmd.Params(0),        { CodFormaPagamento }
                            StringToFloat( Cmd.Params(1)),             { Valor }
                            Cmd.Params(2),                        { Observacao }
                            StrToBoolDef(Trim(Cmd.Params(3)),false) ){ Imp.Vinculado }

        else if Cmd.Metodo = 'fechanaofiscal' then
           FechaNaoFiscal( Cmd.Params(0) )

        else if Cmd.Metodo = 'cancelanaofiscal' then
           CancelaNaoFiscal

        else if (Cmd.Metodo = 'leiturax') or (Cmd.Metodo = 'pafmf_lx_impressao') then
           LeituraX

        else if Cmd.Metodo = 'leituraxserial' then
         begin
           if Cmd.Params(0) <> '' then
              LeituraXSerial(Cmd.Params(0))                         { Nome Arquivo }
           else
            begin
              Linhas := TStringList.Create ;
              try
                 LeituraXSerial( Linhas ) ;                             { Retorno }
                 Cmd.Resposta := Linhas.Text ;
              finally
                 Linhas.Free ;
              end ;
            end;
         end

        else if Cmd.Metodo = 'reducaoz' then
           ReducaoZ( StringToDateTimeDef(Cmd.Params(0),0) )

        else if Cmd.Metodo = 'tipoultimodocumento' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrECFTipoDocumento),Integer(TipoUltimoDocumento))

        else if Cmd.Metodo = 'poucopapel' then
           Cmd.Resposta := BoolToStr( PoucoPapel, true )

        else if Cmd.Metodo = 'horarioverao' then
           Cmd.Resposta := BoolToStr( HorarioVerao, true )

        else if Cmd.Metodo = 'arredonda' then
           Cmd.Resposta := BoolToStr( Arredonda, true )

        else if Cmd.Metodo = 'arredondaporqtd' then
           Cmd.Resposta := BoolToStr( ArredondaPorQtd, true )

        else if Cmd.Metodo = 'arredondaitemmfd' then
           Cmd.Resposta := BoolToStr( ArredondaItemMFD, true )

        else if Cmd.Metodo = 'setarredondaitemmfd' then
           ArredondaItemMFD := StrToBool( Trim(Cmd.Params(0)))

        else if Cmd.Metodo = 'mfd' then
           Cmd.Resposta := BoolToStr( MFD, true )

        else if Cmd.Metodo = 'termica' then
           Cmd.Resposta := BoolToStr( Termica, true )

        else if Cmd.Metodo = 'identificaconsumidorrodape' then
           Cmd.Resposta := BoolToStr( IdentificaConsumidorRodape, True)

        else if Cmd.Metodo = 'estado' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrECFEstado),Integer(Estado))

        else if Cmd.Metodo = 'abregaveta' then
           AbreGaveta

        else if Cmd.Metodo = 'gavetaaberta' then
           Cmd.Resposta := BoolToStr( GavetaAberta, true )

        else if Cmd.Metodo = 'imprimecheque' then
           ImprimeCheque( Cmd.Params(0),                               { Banco }
                          StringToFloat(Cmd.Params(1)),                { Valor }
                          Cmd.Params(2),                          { Favorecido }
                          Cmd.Params(3),                              { Cidade }
                          StringToDateTime(Cmd.Params(4)),                {Data}
                          Cmd.Params(5) )                         { Observação }

        else if Cmd.Metodo = 'cancelaimpressaocheque' then
           CancelaImpressaoCheque

        else if Cmd.Metodo = 'chequepronto' then
           Cmd.Resposta := BoolToStr( ChequePronto, true )

        else if Cmd.Metodo = 'leituracmc7' then
           Cmd.Resposta := LeituraCMC7

        else if Cmd.Metodo = 'mudahorarioverao' then
           if Cmd.Params(0) <> '' then
              MudaHorarioVerao( StrToBool(Trim(Cmd.Params(0))) )
           else
              MudaHorarioVerao( not HorarioVerao )

        else if Cmd.Metodo = 'mudaarredondamento' then
           MudaArredondamento( StrToBool(Trim(Cmd.Params(0))) )

        else if Cmd.Metodo = 'preparatef' then
           PreparaTEF

        else if Cmd.Metodo = 'corrigeestadoerro' then
           CorrigeEstadoErro( StrToBoolDef(Trim(Cmd.Params(0)),True) )

        else if Cmd.Metodo = 'abrerelatoriogerencial' then
           AbreRelatorioGerencial( StrToIntDef(Trim(Cmd.Params(0)), 0 ) ) { Indice }

        else if Cmd.Metodo = 'relatoriogerencial' then
         begin
           Linhas := TStringList.Create ;
           try
              StringToMemo( Cmd.Params(0), Linhas ); {Linha separadas por | (pipe)}
              RelatorioGerencial( Linhas,
                                  StrToIntDef(Trim(Cmd.Params(1)),1) ); { Vias }
           finally
              Linhas.Free ;
           end ;
         end

        else if Cmd.Metodo = 'pulalinhas' then
           PulaLinhas( StrToIntDef( Trim(Cmd.Params(0)),0) )            { Num.Linhas }
           
        else if Cmd.Metodo = 'linharelatoriogerencial' then
         begin
           Linha := StringReplace( Cmd.Params(0),'|',#10,[rfReplaceAll]) ;
           LinhaRelatorioGerencial( Linha )                    { Linha }
         end

        else if Cmd.Metodo = 'abrecupomvinculado' then
           if StringToFloatDef(Cmd.Params(3),-99) <> -99 then { Param 4 é valor ? }
              AbreCupomVinculado( Cmd.Params(0),                         { COO }
                                  Cmd.Params(1),               { CodFormaPagto }
                                  Cmd.Params(2),     { CodComprovanteNaoFiscal }
                                  StringToFloat( Cmd.Params(3) ) )     { Valor }
           else
              AbreCupomVinculado( Cmd.Params(0),                         { COO }
                                  Cmd.Params(1),               { CodFormaPagto }
                                  StringToFloat( Cmd.Params(2) ) )     { Valor }

        else if Cmd.Metodo = 'linhacupomvinculado' then
         begin
           Linha := StringReplace( Cmd.Params(0),'|',#10,[rfReplaceAll]) ;
           LinhaCupomVinculado( Linha )                    { Linha }
         end 

        else if Cmd.Metodo = 'cupomvinculado' then
         begin
           Linhas := TStringList.Create ;
           try
              StringToMemo( Cmd.Params(0), Linhas ); {Linha separadas por | (pipe)}

              if StringToFloatDef(Cmd.Params(3),-99) <> -99 then { Param 4 é valor ? }
                 CupomVinculado( Cmd.Params(0),                          { COO }
                                 Cmd.Params(1),                { CodFormaPagto }
                                 Cmd.Params(2),      { CodComprovanteNaoFiscal }
                                 StringToFloat( Cmd.Params(3) ),       { Valor }
                                 Linhas )
              else
                 CupomVinculado( Cmd.Params(0),                          { COO }
                                 Cmd.Params(1),                { CodFormaPagto }
                                 StringToFloat( Cmd.Params(2) ),       { Valor }
                                 Linhas )
           finally
              Linhas.Free ;
           end ;
         end

        else if Cmd.Metodo = 'estornaccd' then
             Cmd.Resposta := IntToStr(EstornaCCD(StrToBoolDef(Trim(Cmd.Params(0)),true))) {Estorna todos CCD}

        else if Cmd.Metodo = 'segundaviavinculado' then
           SegundaViaVinculado

        else if Cmd.Metodo = 'reimpressaovinculado' then
           ReimpressaoVinculado

        else if Cmd.Metodo = 'fecharelatorio' then
           FechaRelatorio

        else if Cmd.Metodo = 'leituramemoriafiscal' then
           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              LeituraMemoriaFiscal( StringToDateTime(Cmd.Params(0)), {Dt.Inicial}
                                    StringToDateTime(Cmd.Params(1)), {Dt.Final}
                                    StrToBoolDef(Trim(Cmd.Params(2)),False) ) {Simplificada}
           else
              LeituraMemoriaFiscal( StrToInt(Trim(Cmd.Params(0))),  { ReducaoInicial }
                                    StrToInt(Trim(Cmd.Params(1))),    { ReducaoFinal }
                                    StrToBoolDef(Trim(Cmd.Params(2)),False) ) {Simplificada}

        else if Cmd.Metodo = 'leituramemoriafiscalserial' then
         begin
           if Cmd.Params(2) <> '' then
            begin
              if pos(DateSeparator,Cmd.Params(0)) > 0 then
                  LeituraMemoriaFiscalSerial(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),            { Dt.Final }
                      Cmd.Params(2),                              { Nome Arquivo }
                      StrToBoolDef(Trim(Cmd.Params(3)),False) )   {Simplificada}
               else
                  LeituraMemoriaFiscalSerial(
                      StrToInt(Trim(Cmd.Params(0))),              { ReducaoInicial }
                      StrToInt(Trim(Cmd.Params(1))),              { ReducaoFinal }
                      Cmd.Params(2),                              { Nome Arquivo }
                      StrToBoolDef(Trim(Cmd.Params(3)),False) ) ; { Simplificada}
            end
           else
            begin
              Linhas := TStringList.Create ;
              try
                 if pos(DateSeparator,Cmd.Params(0)) > 0 then
                    LeituraMemoriaFiscalSerial(
                        StringToDateTime(Cmd.Params(0)),          { Dt.Inicial }
                        StringToDateTime(Cmd.Params(1)),          { Dt.Final }
                        Linhas,                                   { Retorno }
                        StrToBoolDef(Trim(Cmd.Params(2)),False) ) { Simplificada}
                 else
                    LeituraMemoriaFiscalSerial(
                        StrToInt(Trim(Cmd.Params(0))),            { ReducaoInicial }
                        StrToInt(Trim(Cmd.Params(1))),            { ReducaoFinal }
                        Linhas,                                   { Retorno }
                        StrToBoolDef(Trim(Cmd.Params(2)),False) );{ Simplificada}

                 Cmd.Resposta := Linhas.Text ;
              finally
                 Linhas.Free ;
              end ;
            end;
         end

        else if Cmd.Metodo = 'leituramfdserial' then
         begin
           if Cmd.Params(2) <> '' then
            begin
               if pos(DateSeparator,Cmd.Params(0)) > 0 then
                  LeituraMFDSerial(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),            { Dt.Final }
                      Cmd.Params(2) )                             { Nome do Arquivo }
               else
                  LeituraMFDSerial(
                      StrToInt(Trim(Cmd.Params(0))),              { COOInicial }
                      StrToInt(Trim(Cmd.Params(1))),              { COOFinal }
                      Cmd.Params(2) ) ;                           { Nome do Arquivo }
            end
           else
            begin
              Linhas := TStringList.Create ;
              try
                 if pos(DateSeparator,Cmd.Params(0)) > 0 then
                    LeituraMFDSerial(
                        StringToDateTime(Cmd.Params(0)),          { Dt.Inicial }
                        StringToDateTime(Cmd.Params(1)),          { Dt.Final }
                        Linhas )                                  { Retorno }
                 else
                    LeituraMFDSerial(
                        StrToInt(Trim(Cmd.Params(0))),            { COOInicial }
                        StrToInt(Trim(Cmd.Params(1))),            { COOFinal }
                        Linhas ) ;                                { Retorno }

                 Cmd.Resposta := Linhas.Text ;
              finally
                 Linhas.Free ;
              end ;
            end ;
         end

        else if Cmd.Metodo = 'arquivomfd_dll' then
         begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd ) ;
           TipoDoc := docTodos;             { Valor Padrao para Tipo Documento }
           Finalidade := finMFD;                { Valor Padrao para Finalidade }

           TipoDocStr    := Trim(Cmd.Params(3));
           FinalidadeStr := Trim(Cmd.Params(4));

           if TipoDocStr <> '' then
           begin
              if StrIsNumber(TipoDocStr) then
                 TipoDoc := TACBrECFTipoDocumento(StrToIntDef(TipoDocStr, 19))
              else
                 TipoDoc := TACBrECFTipoDocumento(GetEnumValue(TypeInfo(TACBrECFTipoDocumento),TipoDocStr));   { Tipo de Documento do ArquivoMFD }
           end ;

           if FinalidadeStr <> '' then
           begin
              if StrIsNumber(FinalidadeStr) then
                 Finalidade := TACBrECFFinalizaArqMFD(StrToIntDef( FinalidadeStr, 1))
              else
                 Finalidade := TACBrECFFinalizaArqMFD(GetEnumValue(TypeInfo(TACBrECFFinalizaArqMFD),FinalidadeStr));    { Finalidade do ArquivoMFD }
           end ;

           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              ArquivoMFD_DLL(
                  StringToDateTime(Cmd.Params(0)),                { Dt.Inicial }
                  StringToDateTime(Cmd.Params(1)),                  { Dt.Final }
                  NomeArquivo,                               { Nome do Arquivo }
                  [TipoDoc],                               { Tipo de Documento }
                  Finalidade)                                     { Finalidade }
           else
              ArquivoMFD_DLL(
                  StrToInt(Trim(Cmd.Params(0))),                  { COOInicial }
                  StrToInt(Trim(Cmd.Params(1))),                    { COOFinal }
                  NomeArquivo,                               { Nome do Arquivo }
                  [TipoDoc],                               { Tipo de Documento }
                  Finalidade);                                    { Finalidade }
         end

        else if Cmd.Metodo = 'espelhomfd_dll' then
         begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd ) ;

           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              EspelhoMFD_DLL(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),              { Dt.Final }
                      NomeArquivo )                          { Nome do Arquivo }
           else
              EspelhoMFD_DLL(
                      StrToInt(Trim(Cmd.Params(0))),              { COOInicial }
                      StrToInt(Trim(Cmd.Params(1))),                { COOFinal }
                      NomeArquivo ) ;                        { Nome do Arquivo }
         end

        else if Cmd.Metodo = 'pafmf_lmfc_impressao' then
         begin
           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              PafMF_LMFC_Impressao(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)) )           { Dt.Final }
           else
              PafMF_LMFC_Impressao(
                      StrToInt(Trim(Cmd.Params(0))),              { CRZInicial }
                      StrToInt(Trim(Cmd.Params(1))) ) ;           { CRZFinal }
         end

        else if Cmd.Metodo = 'pafmf_lmfc_espelho' then
         begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd ) ;

           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              PafMF_LMFC_Espelho(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),              { Dt.Final }
                      NomeArquivo )                          { Nome do Arquivo }
           else
              PafMF_LMFC_Espelho(
                      StrToInt(Trim(Cmd.Params(0))),              { CRZInicial }
                      StrToInt(Trim(Cmd.Params(1))),                { CRZFinal }
                      NomeArquivo )                          { Nome do Arquivo }
         end

        else if Cmd.Metodo = 'pafmf_lmfc_cotepe1704' then
         begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd ) ;

           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              PafMF_LMFC_Cotepe1704(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),              { Dt.Final }
                      NomeArquivo )                          { Nome do Arquivo }
           else
              PafMF_LMFC_Cotepe1704(
                      StrToInt(Trim(Cmd.Params(0))),              { CRZInicial }
                      StrToInt(Trim(Cmd.Params(1))),                { CRZFinal }
                      NomeArquivo )                          { Nome do Arquivo }
         end

        else if Cmd.Metodo = 'pafmf_lmfs_impressao' then
         begin
           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              PafMF_LMFS_Impressao(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)) )             { Dt.Final }
           else
              PafMF_LMFS_Impressao(
                      StrToInt(Trim(Cmd.Params(0))),              { CRZInicial }
                      StrToInt(Trim(Cmd.Params(1))) ) ;             { CRZFinal }
         end

        else if Cmd.Metodo = 'pafmf_lmfs_espelho' then
         begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd ) ;

           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              PafMF_LMFS_Espelho(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),              { Dt.Final }
                      NomeArquivo )                          { Nome do Arquivo }
           else
              PafMF_LMFS_Espelho(
                      StrToInt(Trim(Cmd.Params(0))),              { CRZInicial }
                      StrToInt(Trim(Cmd.Params(1))),                { CRZFinal }
                      NomeArquivo )                          { Nome do Arquivo }
         end

        else if Cmd.Metodo = 'pafmf_mfd_espelho' then
         begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd ) ;

           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              PafMF_MFD_Espelho(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),              { Dt.Final }
                      NomeArquivo )                          { Nome do Arquivo }
           else
              PafMF_MFD_Espelho(
                      StrToInt(Trim(Cmd.Params(0))),              { COOInicial }
                      StrToInt(Trim(Cmd.Params(1))),                { COOFinal }
                      NomeArquivo )                          { Nome do Arquivo }
         end

        else if Cmd.Metodo = 'pafmf_mfd_cotepe1704' then
         begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd ) ;

           if pos(DateSeparator,Cmd.Params(0)) > 0 then
              PafMF_MFD_Cotepe1704(
                      StringToDateTime(Cmd.Params(0)),            { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),              { Dt.Final }
                      NomeArquivo )                          { Nome do Arquivo }
           else
              PafMF_MFD_Cotepe1704(
                      StrToInt(Trim(Cmd.Params(0))),              { COOInicial }
                      StrToInt(Trim(Cmd.Params(1))),                { COOFinal }
                      NomeArquivo )                          { Nome do Arquivo }
         end

        else if Cmd.Metodo = 'pafmf_gerarcat52' then
          begin
              PafMF_GerarCAT52(
                      StringToDateTime(Cmd.Params(0)),         { Dt.Inicial }
                      StringToDateTime(Cmd.Params(1)),         { Dt.Final }
                      Cmd.Params(2) )                          { Diretorio Arquivo }
          end

        else if Cmd.Metodo = 'assinarblocoxestoque' then
          begin
            if FileExists(Cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));
                FXMLOriginal.Text := FrmACBrMonitor.ACBrBlocoX1.SSL.Assinar(FXMLOriginal.Text, 'Estoque', 'Mensagem');
                FXMLOriginal.SaveToFile(Cmd.Params(0));
                Cmd.Resposta:= 'OK: '+ Cmd.Params(0);
              finally
                FXMLOriginal.Free;;
              end;
            end
            else
                Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.SSL.Assinar(Cmd.Params(0), 'Estoque', 'Mensagem');
          end
        else if Cmd.Metodo = 'assinarblocoxreducaoz' then
          begin
            if FileExists(cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));
                FXMLOriginal.Text := FrmACBrMonitor.ACBrBlocoX1.SSL.Assinar(FXMLOriginal.Text, 'ReducaoZ', 'Mensagem');
                FXMLOriginal.SaveToFile(Cmd.Params(0));
                Cmd.Resposta:= 'OK: '+ Cmd.Params(0);
              finally
                FXMLOriginal.Free;
              end;
            end
            else
               Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.SSL.Assinar(Cmd.Params(0), 'ReducaoZ', 'Mensagem');
          end
        else if Cmd.Metodo = 'assinarblocox' then
          begin
            if FileExists(cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));

                if (Pos('</reducaoz>',LowerCase(FXMLOriginal.Text)) > 0) then
                  FXMLOriginal.Text := FrmACBrMonitor.ACBrBlocoX1.SSL.Assinar(FXMLOriginal.Text, 'ReducaoZ', 'Mensagem')
                else
                  FXMLOriginal.Text := FrmACBrMonitor.ACBrBlocoX1.SSL.Assinar(FXMLOriginal.Text, 'Estoque', 'Mensagem');
                FXMLOriginal.SaveToFile(Cmd.Params(0));
                Cmd.Resposta:= 'OK: '+ Cmd.Params(0);
              finally
                FXMLOriginal.Free;
              end;
            end
            else
               Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.SSL.Assinar(Cmd.Params(0), 'ReducaoZ', 'Mensagem');
          end

        else if Cmd.Metodo = 'validarblocoxestoque' then
          begin
            if FileExists(cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));
                FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.XML := FXMLOriginal.Text;
              finally
                FXMLOriginal.Free;;
              end;
            end
            else
               FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.XML := Cmd.Params(0);

            FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.ValidarPafEcfEEcf := False;
            FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.Executar;
            Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.RetWS;
          end

        else if Cmd.Metodo = 'validarblocoxreducaoz' then
          begin
            if FileExists(cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));
                FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.XML := FXMLOriginal.Text;
              finally
                FXMLOriginal.Free;
              end;
            end
            else
               FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.XML := Cmd.Params(0);

            FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.ValidarPafEcfEEcf := False;
            FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.Executar;
            Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.RetWS;
          end

        else if Cmd.Metodo = 'validarblocox' then
          begin
            if FileExists(cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));
                FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.XML := FXMLOriginal.Text;
              finally
                FXMLOriginal.Free;
              end;
            end
            else
               FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.XML := Cmd.Params(0);

            FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.ValidarPafEcfEEcf := False;
            FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.Executar;
            Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.WebServices.ValidarBlocoX.RetWS;
          end

        else if Cmd.Metodo = 'enviarblocoxestoque' then
          begin
            if FileExists(cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));
                FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.XML := FXMLOriginal.Text;
              finally
                FXMLOriginal.Free;
              end;
            end
            else
               FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.XML := Cmd.Params(0);

            FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.Executar;
            Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.RetWS;
          end

        else if Cmd.Metodo = 'enviarblocoxreducaoz' then
          begin
            if FileExists(cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));
                FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.XML := FXMLOriginal.Text;
              finally
                FXMLOriginal.Free;
              end;
            end
            else
              FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.XML := Cmd.Params(0);

            FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.Executar;
            Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.RetWS;
          end

        else if Cmd.Metodo = 'enviarblocox' then
          begin
            if FileExists(cmd.Params(0)) then
            begin
              FXMLOriginal := TStringList.Create;
              try
                FXMLOriginal.LoadFromFile(Cmd.Params(0));
                FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.XML := FXMLOriginal.Text;
              finally
                FXMLOriginal.Free;
              end;
            end
            else
               FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.XML := Cmd.Params(0);

            FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.Executar;
            Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.WebServices.EnviarBlocoX.RetWS;
          end

        else if Cmd.Metodo = 'consultarblocox' then
          begin
            FrmACBrMonitor.ACBrBlocoX1.WebServices.ConsultarBlocoX.Recibo:= Cmd.Params(0);
            FrmACBrMonitor.ACBrBlocoX1.WebServices.ConsultarBlocoX.Executar;
            Cmd.Resposta := FrmACBrMonitor.ACBrBlocoX1.WebServices.ConsultarBlocoX.RetWS;
          end

        else if Cmd.Metodo = 'enviacomando' then
          begin
             if Cmd.Params(1) <> '' then
                EnviaComando(Cmd.Params(0),StrToInt(Trim(Cmd.Params(1))))
             else
                EnviaComando(Cmd.Params(0))
          end

        else if Cmd.Metodo = 'assinaarquivo' then
        begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd, 0 ) ;
           AssinaArquivoComEAD(Cmd.Params(0))
        end

        else if Cmd.Metodo = 'configbarras' then
        begin
          if StrToIntDef(Trim(Cmd.Params(0)),0) > 0 then
                    ConfigBarras.Altura:= StrToInt(Trim(Cmd.Params(0)));
          if StrToIntDef(Trim(Cmd.Params(1)),0) > 0 then
                    ConfigBarras.LarguraLinha := StrToInt(Trim(Cmd.Params(1)));
        end

        else if (Cmd.Metodo = 'pafmf_arqmf') or (Cmd.Metodo = 'pafmf_arquivomf') or (Cmd.Metodo = 'pafmf_arqmf_binario')then
        begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd, 0 ) ;
           PafMF_ArqMF_Binario(Cmd.Params(0));
        end

        else if (Cmd.Metodo = 'pafmf_arqmfd') or (Cmd.Metodo = 'pafmf_arquivomfd') or (Cmd.Metodo = 'pafmf_arqmfd_binario')then
        begin
           NomeArquivo := AjustaNomeArquivoCmd( Cmd, 0 ) ;
           PafMF_ArqMFD_Binario(Cmd.Params(0));
        end

        ELSE
           raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;

     finally
       { Nada a fazer aqui por enquanto... :) }
     end ;
  end ;
end ;

{------------------------------------------------------------------------------}
Function PegaAliquotas : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if Aliquotas.Count < 1 then
        CarregaAliquotas ;

     for I := 0 to Aliquotas.Count -1 do
        Result := Result + PadLeft(Aliquotas[I].Indice,4) +
                           Aliquotas[I].Tipo +
                           FormatFloat('##0.00', Aliquotas[I].Aliquota ) + '|' ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

function PegaRelatoriosGerenciais : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if RelatoriosGerenciais.Count < 1 then
        CarregaRelatoriosGerenciais ;

     for I := 0 to RelatoriosGerenciais.Count -1 do
        Result := Result + PadLeft(RelatoriosGerenciais[I].Indice,4) +
                           PadLeft( RelatoriosGerenciais[I].Descricao, 30) +
                           IntToStrZero( RelatoriosGerenciais[I].Contador, 5 ) + '|' ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end;

function PegaTotaisRelatoriosGerenciais : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     LerTotaisRelatoriosGerenciais ;

     for I := 0 to RelatoriosGerenciais.Count -1 do
     begin
        Result := Result + PadLeft( RelatoriosGerenciais[I].Indice,4)  +
                           IntToStrZero( RelatoriosGerenciais[I].Contador, 5 ) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end;

{------------------------------------------------------------------------------}
Function PegaTotaisAliquotas : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     LerTotaisAliquota ;

     for I := 0 to Aliquotas.Count -1 do
        Result := Result + PadLeft(Aliquotas[I].Indice,4) +
                           FormatFloat('########0.00', Aliquotas[I].Total ) + '|' ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
Function PegaFormasPagamento : String ;
Var I : Integer ;
    Vinc : Char ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if FormasPagamento.Count < 1 then
        CarregaFormasPagamento ;

     for I := 0 to FormasPagamento.Count -1 do
     begin
        Vinc := ' ' ;
        if FormasPagamento[I].PermiteVinculado then
           Vinc := 'V' ;

        Result := Result + PadLeft( FormasPagamento[I].Indice,4) + Vinc +
                           PadLeft( FormasPagamento[I].Descricao, 30) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
Function PegaTotaisFormasPagamento : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     LerTotaisFormaPagamento ;
     
     for I := 0 to FormasPagamento.Count -1 do
     begin
        Result := Result + PadLeft( FormasPagamento[I].Indice,4)  +
                           FormatFloat('########0.00', FormasPagamento[I].Total ) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
Function PegaComprovantesNaoFiscais : String ;
Var I : Integer ;
    Vinc : Char ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if ComprovantesNaoFiscais.Count < 1 then
        CarregaComprovantesNaoFiscais ;

     for I := 0 to ComprovantesNaoFiscais.Count -1 do
     begin
        Vinc := ' ' ;
        if ComprovantesNaoFiscais[I].PermiteVinculado then
           Vinc := 'V' ;

        Result := Result + PadLeft( ComprovantesNaoFiscais[I].Indice,4) + Vinc +
                           PadLeft( ComprovantesNaoFiscais[I].Descricao,30) +
                           PadLeft( ComprovantesNaoFiscais[I].FormaPagamento,4) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
Function PegaTotaisComprovantesNaoFiscais : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     LerTotaisComprovanteNaoFiscal ;
     
     for I := 0 to ComprovantesNaoFiscais.Count -1 do
     begin
        Result := Result + PadLeft( ComprovantesNaoFiscais[I].Indice,4)  +
                           FormatFloat('########0.00', ComprovantesNaoFiscais[I].Total ) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
Function PegaUnidadesMedida : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrECF1 {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if UnidadesMedida.Count < 1 then
        CarregaUnidadesMedida ;

     for I := 0 to UnidadesMedida.Count -1 do
        Result := Result + PadLeft( UnidadesMedida[I].Indice,4) +
                           PadLeft( UnidadesMedida[I].Descricao,4) + '|' ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
Procedure StringToMemo( AString : AnsiString; Memo : TStringList );
begin
  AString   := StringReplace(AString,#13+#10,'|',[rfReplaceAll]) ;
  AString   := StringReplace(AString,#10,'|',[rfReplaceAll]) ;
  Memo.Text := StringReplace(AString,'|',sLineBreak,[rfReplaceAll]) ;
end ;

end.


