{******************************************************************************}

{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2019 Daniel Simoes de Almeida               }
{                                       Juliomar Marchetti                     }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/03/2019: Juliomar Marchetti
|*  - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
******************************************************************************}

{$I ACBr.inc}

unit ACBrTEFDCappta;


interface

uses
  Classes, SysUtils, ACBrTEFDClass
  {$IfNDef NOGUI}
    {$If DEFINED(VisualCLX)}
  , QControls
    {$ElseIf DEFINED(FMX)}
  , System.UITypes
    {$ElseIf DEFINED(DELPHICOMPILER16_UP)}
  , System.UITypes
    {$Else}
  , Controls
    {$IfEnd}
  {$EndIf};

const
  CACBrTEFD_Cappta_ImprimeGerencialConcomitante = False ;
  CACBrTEFD_Cappta_PressioneEnter = 'PRESSIONE <ENTER>' ;
  CACBrTEFD_Cappta_TransacaoNaoEfetuada = 'Transação não efetuada.' ;
  CACBrTEFD_Cappta_TransacaoNaoEfetuadaReterCupom =
     'Transação não efetuada.'+sLineBreak+'Favor reter o Cupom' ;
  CACBrTEFD_Cappta_TransacaoEfetuadaReImprimir =
     'Transação TEF efetuada.'        + sLineBreak+
     'Favor reimprimir último Cupom.' + sLineBreak +
     '%s'                             + sLineBreak +
     '(Para Cielo utilizar os 6 últimos dígitos.)';
  CACBrTEFD_Cappta_NaoInicializado = 'Cappta não inicializado' ;
  CACBrTEFD_Cappta_NaoConcluido = 'Requisição anterior não concluida' ;
  CACBrTEFD_Cappta_Erro1  = 'Não autenticado/Alguma das informações fornecidas para autenticação não é válida' ;
  CACBrTEFD_Cappta_Erro2  = 'CapptaGpPlus está sendo inicializado' ;
  CACBrTEFD_Cappta_Erro3  = 'Código de terminal inválido' ;
  CACBrTEFD_Cappta_Erro6  = 'Erro na inicialização do TCP/IP' ;
  CACBrTEFD_Cappta_Erro7  = 'Erro interno no CapptaGpPlus' ;
  CACBrTEFD_Cappta_Erro8  = 'Erro na comunicação entre a CappAPI e o CapptaGpPlus' ;
  CACBrTEFD_Cappta_Erro9  = 'Ocorre quando qualquer operação é realizada sem que se tenha finalizado o último pagamento';
  CACBrTEFD_Cappta_Erro10 = 'Uma reimpressão ou cancelamento foi executada dentro de uma sessão multi-cartões';
  CACBrTEFD_Cappta_Erro11 = 'Dados inválidos passados pela automação.';
  CACBrTEFD_Cappta_Erro12 = 'Modo seguro não ativo (possível falta de configuração no servidor SiTef do arquivo .cha).';
  CACBrTEFD_Cappta_Erro13 = 'Caminho da DLL inválido (o caminho completo das bibliotecas está muito grande).';
  CACBrTEFD_Cappta_Erro14 = 'Valor digitado no pinpad é inválido.';


  {$IFDEF LINUX}
    CACBrTEFD_Cappta_Lib = 'Cappta.Gp.Api.Com.so' ;
  {$ELSE}
    CACBrTEFD_Cappta_Lib = 'Cappta.Gp.Api.Com.dll' ;
  {$ENDIF}

{ TACBrTEFDCappta }
type
  TACBrTEFDCappta = class(TACBrTEFDClass)
  private
    xAutenticarPdv : function(
      cnpj : PAnsiChar;
      pdv : integer;
      chaveAutenticacao: PAnsiChar):integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xCancelarPagamento : function(
      senhaAdministrativa : PAnsiChar;
      numeroControle: PAnsiChar): integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    //xConfigurar : function (
      //Cappta.Gp.Api.Com.Model.IConfiguracoes configs) : integer;
    //{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xConfirmarPagamentos : function : integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xDesfazerPagamentos : function : integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xEnviarParametro : function(
      parametro : PAnsiChar;
      acao : integer) : integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xIniciarMultiCartoes : function (
      quantidadePagamentos : integer) : integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    //xIterarOperacaoTef: function : Cappta.Gp.Api.Com.Model.IIteracaoTef;
    //{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    //xPagamentoCrediario : function (
      //valor : double;
      //detalhes : Cappta.Gp.Api.Com.Model.IDetalhesCrediario ):integer;
    //{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    //xPagamentoCredito : function(
      //valor: double;
      //detalhes: Cappta.Gp.Api.Com.Model.IDetalhesCredito ) : integer;
    //{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xPagamentoDebito : function (
      valor: double) : integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    //xPagamentoTicketCarPessoaFisica : function(
      //valor : double;
      //detalhes : Cappta.Gp.Api.Com.Model.IDetalhesPagamentoTicketCarPessoaFisica ): integer;
    //{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xReimprimirCupom : function(
      numeroControle: PAnsiChar;
      tipoVia : integer): integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xReimprimirUltimoCupom: function(
      tipoVia: integer): integer;
    {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    //xSolicitarInformacoesPinpad : function(
      //requisicaoPinpad : Cappta.Gp.Api.Com.Model.IRequisicaoInformacaoPinpad ): PAnsiChar;
    //{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;


    fPathDLL: string;

    procedure LoadDLLFunctions;
    procedure UnLoadDLLFunctions;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Inicializar ; override;
    procedure DesInicializar ; override;

    procedure AtivarGP ; override;
    procedure VerificaAtivo ; override;

    property PathDLL: string read fPathDLL write fPathDLL;

  published
  end;

implementation

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  dateutils, Math, StrUtils,
  ACBrTEFD, ACBrUtil;

{ TACBrTEFDCappta }

procedure TACBrTEFDCappta.LoadDLLFunctions;
  procedure CapptaFunctionDetect( FuncName: AnsiString; var LibPointer: Pointer ) ;
   var
   sLibName: string;
   begin
     if not Assigned( LibPointer )  then
     begin
       // Verifica se exite o caminho das DLLs
       sLibName := '';
       if Length(PathDLL) > 0 then
          sLibName := PathWithDelim(PathDLL);

       // Concatena o caminho se exitir mais o nome da DLL.
       sLibName := sLibName + CACBrTEFD_Cappta_Lib;

       if not FunctionDetect( sLibName, FuncName, LibPointer) then
       begin
          LibPointer := NIL ;
          raise EACBrTEFDErro.Create( ACBrStr( 'Erro ao carregar a funo:'+FuncName+
                                           ' de: '+CACBrTEFD_Cappta_Lib ) ) ;
       end ;
     end ;
   end ;
begin
  CapptaFunctionDetect('AutenticarPdv',@xAutenticarPdv);
  CapptaFunctionDetect('CancelarPagamento', @xCancelarPagamento );
  //CapptaFunctionDetect('Configurar', @xConfigurar);
  CapptaFunctionDetect('ConfirmarPagamentos', @xConfirmarPagamentos);
  CapptaFunctionDetect('DesfazerPagamentos', @xDesfazerPagamentos);
  CapptaFunctionDetect('EnviarParametro', @xEnviarParametro );
  CapptaFunctionDetect('IniciarMultiCartoes', @xIniciarMultiCartoes);
  //CapptaFunctionDetect('IterarOperacaoTef', @xIterarOperacaoTef);
  //CapptaFunctionDetect('PagamentoCrediario', @xPagamentoCrediario);
  //CapptaFunctionDetect('PagamentoCredito', @xPagamentoCredito);
  CapptaFunctionDetect('PagamentoDebito', @xPagamentoDebito);
  //CapptaFunctionDetect('PagamentoTicketCarPessoaFisica', @xPagamentoTicketCarPessoaFisica);
  CapptaFunctionDetect('ReimprimirCupom', @xReimprimirCupom);
  CapptaFunctionDetect('ReimprimirUltimoCupom', @xReimprimirUltimoCupom);
  //CapptaFunctionDetect('SolicitarInformacoesPinpad', @xSolicitarInformacoesPinpad);

end;

procedure TACBrTEFDCappta.UnLoadDLLFunctions;
 var
    sLibName: String;
 begin
   sLibName := '';

   if Length(PathDLL) > 0 then
      sLibName := PathWithDelim(PathDLL);

   UnLoadLibrary( sLibName + CACBrTEFD_Cappta_Lib );

   xAutenticarPdv := nil;
   xCancelarPagamento := nil;
   //xConfigurar : = nil;
   xConfirmarPagamentos := nil;
   xDesfazerPagamentos := nil;
   xEnviarParametro := nil;
   xIniciarMultiCartoes := nil;
   //xIterarOperacaoTef:= nil;
   //xPagamentoCrediario := nil;
   //xPagamentoCredito := nil;
   xPagamentoDebito := nil;
   //xPagamentoTicketCarPessoaFisica := nil;
   xReimprimirCupom := nil;
   xReimprimirUltimoCupom:= nil;
   //xSolicitarInformacoesPinpad := nil;

end;

constructor TACBrTEFDCappta.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ArqReq    := '' ;
  ArqResp   := '' ;
  ArqSTS    := '' ;
  ArqTemp   := '' ;
  GPExeName := '' ;
  fpTipo    := gpCappta;
  Name      := 'Cappta' ;

  if Assigned( fpResp ) then
     fpResp.Free ;

  fpResp := TACBrTEFDRespCliSiTef.Create;
  fpResp.TipoGP := Tipo;
end;

destructor TACBrTEFDCappta.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrTEFDCappta.Inicializar;
 Var
   Sts : Integer ;
   ParamAdic : AnsiString ;
   Erro : String;
 begin
   if Inicializado then exit ;


   LoadDLLFunctions;


   //GravaLog( '*** ConfiguraIntSiTefInterativoEx. EnderecoIP: '   +fEnderecoIP+
   //                                          ' CodigoLoja: '     +fCodigoLoja+
   //                                          ' NumeroTerminal: ' +fNumeroTerminal+
   //                                          ' Resultado: 0'     +
   //                                          ' ParametrosAdicionais: '+ParamAdic ) ;

   //Sts := xConfiguraIntSiTefInterativoEx( PAnsiChar(fEnderecoIP),
   //                                       PAnsiChar(fCodigoLoja),
   //                                       PAnsiChar(fNumeroTerminal),
   //                                       0,
   //                                       PAnsiChar(ParamAdic) );
   //Erro := '' ;
   //Case Sts of
   //  1 :	Erro := CACBrTEFD_CliSiTef_Erro1;
   //  2 : Erro := CACBrTEFD_CliSiTef_Erro2;
   //  3 : Erro := CACBrTEFD_CliSiTef_Erro3;
   //  6 : Erro := CACBrTEFD_CliSiTef_Erro6;
   //  7 : Erro := CACBrTEFD_CliSiTef_Erro7;
   //  8 : Erro := CACBrTEFD_CliSiTef_Erro8;
   // 10 : Erro := CACBrTEFD_CliSiTef_Erro10;
   // 11 : Erro := CACBrTEFD_CliSiTef_Erro11;
   // 12 : Erro := CACBrTEFD_CliSiTef_Erro12;
   // 13 : Erro := CACBrTEFD_CliSiTef_Erro13;
   //end;

   if Erro <> '' then
      raise EACBrTEFDErro.Create( ACBrStr( Erro ) ) ;

   GravaLog( Name +' Inicializado Cappta' );

   VerificarTransacoesPendentesClass(True);
   fpInicializado := True;
end;

procedure TACBrTEFDCappta.DesInicializar;
begin
  UnLoadDLLFunctions ;

  inherited DesInicializar;
end;

procedure TACBrTEFDCappta.AtivarGP;
begin
  raise EACBrTEFDErro.Create( ACBrStr( 'Cappta não pode ser ativado localmente' )) ;
end;

procedure TACBrTEFDCappta.VerificaAtivo;
begin
  {Nada a Fazer}
end;

end.

