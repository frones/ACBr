{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
|* 18/03/2011: Márcio Delfino Carvalho
|*  - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
|* 10/08/2011: Márcio Delfino Carvalho
|*  - Exibição da msg "IMPRIMINDO", durante a impressão
******************************************************************************}


{$I ACBr.inc}

unit ACBrTEFDBanese;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, ACBrTEFDClass
  {$IfNDef NOGUI}
    {$If DEFINED(VisualCLX)}
      ,QControls, QForms
    {$ElseIf DEFINED(FMX)}
      ,System.UITypes, FMX.Forms
    {$ElseIf DEFINED(DELPHICOMPILER16_UP)}
      ,System.UITypes, Vcl.Forms
    {$Else}
      ,Controls, Forms
    {$IfEnd}
  {$EndIf};


Const
  {Caminho padrão dos arquivos de requisição e resposta}
  CACBrTEFDBanese_ArqTemp    = 'C:\bcard\req\pergunta.tmp' ;
  CACBrTEFDBanese_ArqReq     = 'C:\bcard\req\pergunta.txt' ;
  CACBrTEFDBanese_ArqResp    = 'C:\bcard\resp\resposta.txt' ;
  CACBrTEFDBanese_ArqRespBkp = 'C:\bcard\resposta.txt' ;
  CACBrTEFDBanese_ArqRespMovBkp = 'C:\bcard\copiamovimento.txt' ;
  CACBrTEFDBanese_ArqSTS     = 'C:\bcard\resp\status.txt' ;

type
  { TACBrTEFDRespBanese }

  TACBrTEFDRespBanese = class( TACBrTEFDResp )
  protected
    function GetTransacaoAprovada : Boolean; override;
  public
    procedure ConteudoToProperty; override;
    procedure GravaInformacao( const Identificacao : Integer;
      const Informacao : AnsiString );
  end;

  {Evento para obter informações sobre o tipo de transação ADM}
  TACBrTEFDBaneseObtemInformacao = procedure( var ItemSelecionado : Integer ) of object ;

  {Tipos de operação}
  TOperacaoTEFBanese = (operCRT, operReimpressao, operCancelamento, operFechamento) ;

  { TACBrTEFDBanese }

   TACBrTEFDBanese = class( TACBrTEFDClass )
   private
     fArqReq : String;
     fArqTmp : String;
     fArqResp: String;
     fArqSTS : String;
     fArqRespBkp: String;
     fRespostas: TStringList;
     fDocumentosProcessados : AnsiString ;
     fOnObtemInformacao : TACBrTEFDBaneseObtemInformacao;
     fArqRespMovBkp: String;
     procedure SetArqTmp(const AValue : String);
     procedure SetArqReq(const AValue : String);
     procedure SetArqResp(const AValue : String);
     procedure SetArqSTS(const AValue : String);
     procedure SetArqRespBkp(const AValue : String);
     procedure MontaArquivoResposta(aNSU: String; aRetorno:TStringList;
       operacao:TOperacaoTEFBanese);
     procedure ImprimirComprovantes(SL : TStringList);
     function  RealizaTransacao(operacao : TOperacaoTEFBanese; Valor : Double): Boolean;
     function  ProcessaRespostaRequisicao(operacao: TOperacaoTEFBanese) : Boolean;
     procedure SetArqRespMovBkp(const Value: String);
   protected
     Function  FazerRequisicao(AHeader : AnsiString = ''; Valor : Double = 0;
       IndiceFPG_ECF : String = '') : Boolean ;
     Function ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean; override;
   public
     property Respostas : TStringList read fRespostas ;

     constructor Create(AOwner : TComponent); override;
     destructor Destroy ; override;

     procedure AtivarGP ; override;
     procedure VerificaAtivo ; override;

     procedure ConfirmarEReimprimirTransacoesPendentes ;

     Function ADM : Boolean ; override;
     Function CRT( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean; override;
     Function CHQ( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; CMC7 : String = '';
        TipoPessoa : AnsiChar = 'F'; DocumentoPessoa : String = '';
        DataCheque : TDateTime = 0; Banco   : String = '';
        Agencia    : String = ''; AgenciaDC : String = '';
        Conta      : String = ''; ContaDC   : String = '';
        Cheque     : String = ''; ChequeDC  : String = '';
        Compensacao: String = '' ) : Boolean ; override;
     Procedure NCN(Rede, NSU, Finalizacao : String;
        Valor : Double = 0; DocumentoVinculado : String = ''); override;
     Procedure CNF(Rede, NSU, Finalizacao : String;
        DocumentoVinculado : String = ''); override;
     Function CNC(Rede, NSU : String; DataHoraTransacao : TDateTime;
        Valor : Double) : Boolean; overload; override;
   published
     property ArqTemp  : String read fArqTmp    write SetArqTmp ;
     property ArqReq   : String read fArqReq    write SetArqReq ;
     property ArqSTS   : String read fArqSTS    write SetArqSTS  ;
     property ArqResp  : String read fArqResp   write SetArqResp ;
     property ArqRespBkp  : String read fArqRespBkp   write SetArqRespBkp ;
     property ArqRespMovBkp  : String read fArqRespMovBkp   write SetArqRespMovBkp ;
     property OnObtemInformacao : TACBrTEFDBaneseObtemInformacao read fOnObtemInformacao write fOnObtemInformacao ;
   end;

implementation

Uses dateutils, strutils, math,
  ACBrTEFD, ACBrUtil;

{ TACBrTEFDRespBanese }

function TACBrTEFDRespBanese.GetTransacaoAprovada : Boolean;
begin
   Result := True ;
end;

procedure TACBrTEFDRespBanese.ConteudoToProperty;
var
   Linha : TACBrTEFDLinha ;
   I     : Integer;
   Parc  : TACBrTEFDRespParcela;
   LinStr: AnsiString ;
begin
   fpValorTotal := 0 ;
   fpImagemComprovante1aVia.Clear;
   fpImagemComprovante2aVia.Clear;

   for I := 0 to Conteudo.Count - 1 do
   begin
     Linha  := Conteudo.Linha[I];
     LinStr := StringToBinaryString( Linha.Informacao.AsString );

     case Linha.Identificacao of
       100 :fpModalidadePagto              := LinStr;
       101 :fpModalidadePagtoExtenso       := LinStr;
       102 :fpModalidadePagtoDescrita      := LinStr;
       105 :
         begin
           fpDataHoraTransacaoComprovante  := Linha.Informacao.AsTimeStampSQL;
           fpDataHoraTransacaoHost         := fpDataHoraTransacaoComprovante ;
         end;
       121 : fpImagemComprovante1aVia.Text := StringToBinaryString( Linha.Informacao.AsString );
       122 : fpImagemComprovante2aVia.Text := StringToBinaryString( Linha.Informacao.AsString );
       130 :
         begin
           fpSaque      := Linha.Informacao.AsFloat ;
           fpValorTotal := fpValorTotal + fpSaque ;
         end;
       131 : fpInstituicao                 := LinStr;
       133 : fpCodigoAutorizacaoTransacao  := Linha.Informacao.AsString;
       134 : fpNSU                         := Linha.Informacao.AsString;
       //156 : fpRede                        := LinStr;
       501 : fpTipoPessoa                  := AnsiChar(IfThen(Linha.Informacao.AsInteger = 0,'J','F')[1]);
       502 : fpDocumentoPessoa             := LinStr ;
       505 : fpQtdParcelas                 := Linha.Informacao.AsInteger ;
       506 : fpDataPreDatado               := Linha.Informacao.AsDate;

       //incluido por Evandro
       627 : fpAgencia                     := LinStr;
       628 : fpAgenciaDC                   := LinStr;
       120 : fpAutenticacao                := LinStr;
       626 : fpBanco                       := LinStr;
       613 :
        begin
          fpCheque                         := copy(LinStr, 21, 6);
          fpCMC7                           := LinStr;
        end;
       629 : fpConta                       := LinStr;
       630 : fpContaDC                     := LinStr;
       527 : fpDataVencimento              := Linha.Informacao.AsDate ; {Data Vencimento}

       //

       899 :  // Tipos de Uso Interno do ACBrTEFD
        begin
          case Linha.Sequencia of
              1 : fpCNFEnviado         := (UpperCase( Linha.Informacao.AsString ) = 'S' );
              2 : fpIndiceFPG_ECF      := Linha.Informacao.AsString ;
              3 : fpOrdemPagamento     := Linha.Informacao.AsInteger ;
            100 : fpHeader             := LinStr;
            101 : fpID                 := Linha.Informacao.AsInteger;
            102 : fpDocumentoVinculado := LinStr;
            103 : fpValorTotal         := fpValorTotal + Linha.Informacao.AsFloat;
            104 : fpRede               := Linha.Informacao.AsString ;
            130 : fpTextoEspecialOperador := Linha.Informacao.AsString;
          end;
        end;
     end;
   end ;

   // 1 a 2 via são iguais //
   fpImagemComprovante2aVia.AddStrings( fpImagemComprovante1aVia );

   fpParcelas.Clear;
   for I := 1 to fpQtdParcelas do
   begin
      Parc := TACBrTEFDRespParcela.create;
      Parc.Vencimento := LeInformacao( 141, I).AsDate ;
      Parc.Valor      := LeInformacao( 142, I).AsFloat ;

      fpParcelas.Add(Parc);
   end;
end;

procedure TACBrTEFDRespBanese.GravaInformacao(const Identificacao : Integer;
   const Informacao : AnsiString);
begin
  fpConteudo.GravaInformacao( Identificacao, 0,
                              BinaryStringToString(Informacao) ); // Converte #10 para "\x0A"
end;

{ TACBrTEFDClass }

constructor TACBrTEFDBanese.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ArqReq    := CACBrTEFDBanese_ArqReq ;
  ArqResp   := CACBrTEFDBanese_ArqResp ;
  ArqSTS    := CACBrTEFDBanese_ArqSTS ;
  ArqTemp   := CACBrTEFDBanese_ArqTemp ;
  ArqRespBkp  := CACBrTEFDBanese_ArqRespBkp ;
  ArqRespMovBkp := CACBrTEFDBanese_ArqRespMovBkp ;  
  GPExeName := '' ;
  fpTipo    := gpBanese;
  Name      := 'Banese' ;
end;

destructor TACBrTEFDBanese.Destroy;
begin
   fRespostas.Free ;

   inherited Destroy;
end;

procedure TACBrTEFDBanese.SetArqTmp(const AValue : String);
begin
  fArqTmp := Trim( AValue ) ;
end;

procedure TACBrTEFDBanese.SetArqReq(const AValue : String);
begin
  fArqReq := Trim( AValue ) ;
end;

procedure TACBrTEFDBanese.SetArqResp(const AValue : String);
begin
  fArqResp := Trim( AValue ) ;
end;

procedure TACBrTEFDBanese.SetArqSTS(const AValue : String);
begin
  fArqSTS := Trim( AValue ) ;
end;

procedure TACBrTEFDBanese.SetArqRespBkp(const AValue : String);
begin
  fArqRespBkp := Trim( AValue ) ;
end;


procedure TACBrTEFDBanese.AtivarGP;
begin
   raise EACBrTEFDErro.Create( ACBrStr( 'CliBanese não pode ser ativado localmente' )) ;
end;

procedure TACBrTEFDBanese.VerificaAtivo;
begin
   {Nada a Fazer}
end;

procedure TACBrTEFDBanese.ConfirmarEReimprimirTransacoesPendentes;
Var
  ArquivosVerficar : TStringList ;
  ArqMask, NSUs    : AnsiString;
  ExibeMsg         : Boolean ;
begin
  ArquivosVerficar := TStringList.Create;

  try
     ArquivosVerficar.Clear;

     { Achando Arquivos de Backup deste GP }
     ArqMask := TACBrTEFD(Owner).PathBackup + PathDelim + 'ACBr_' + Self.Name + '_*.tef' ;
     FindFiles( ArqMask, ArquivosVerficar, True );
     NSUs := '' ;
     ExibeMsg := (ArquivosVerficar.Count > 0) ;

     { Enviando NCN ou CNC para todos os arquivos encontrados }
     while ArquivosVerficar.Count > 0 do
     begin
        if not FileExists( ArquivosVerficar[ 0 ] ) then
        begin
           ArquivosVerficar.Delete( 0 );
           Continue;
        end;

        Resp.LeArquivo( ArquivosVerficar[ 0 ] );

        try
           if pos(Resp.DocumentoVinculado, fDocumentosProcessados) = 0 then
              CNF(Resp.Rede, Resp.NSU, '', '');   {Confirma}

           if Resp.NSU <> '' then
              NSUs := NSUs + sLineBreak + 'NSU: '+Resp.NSU ;

           DeleteFile( ArquivosVerficar[ 0 ] );
           ArquivosVerficar.Delete( 0 );
        except
        end;
     end;

     if ExibeMsg then
        TACBrTEFD(Owner).DoExibeMsg( opmOK,
                               'Transação TEF efetuada.'+sLineBreak+
                               'Favor Re-Imprimir Ultimo Cupom ' + NSUs ) ;

  finally
     ArquivosVerficar.Free;
  end;
end;

Function TACBrTEFDBanese.ADM : Boolean;
begin
  Result := FazerRequisicao('ADM', 0, '0' );
end;

Function TACBrTEFDBanese.CRT( Valor : Double; IndiceFPG_ECF : String;
   DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean;
begin
  Result := False;

  VerificarTransacaoPagamento( Valor );

  if FazerRequisicao('CRT', Valor, DocumentoVinculado ) then
    begin
      ProcessarRespostaPagamento( IndiceFPG_ECF, Valor );
      Result := True;
    end;
end;

Function TACBrTEFDBanese.CHQ(Valor : Double; IndiceFPG_ECF : String;
   DocumentoVinculado : String; CMC7 : String; TipoPessoa : AnsiChar;
   DocumentoPessoa : String; DataCheque : TDateTime; Banco : String;
   Agencia : String; AgenciaDC : String; Conta : String; ContaDC : String;
   Cheque : String; ChequeDC : String; Compensacao: String) : Boolean ;
begin
  Result := False;
end;

Procedure TACBrTEFDBanese.CNF(Rede, NSU, Finalizacao : String;
   DocumentoVinculado : String) ;
var ArquivoReq : TStringList;   
begin
  {O CNF é sempre padrão, não necessitando ser montado dinamicamente}
  ArquivoReq := TStringList.Create;
  ArquivoReq.Add('CF0000TTTT00');
  ArquivoReq.SaveToFile(ArqTemp);
  Sleep(1000);
  RenameFile(ArqTemp, ArqReq);
  Sleep(2000);
  DeleteFile(ArqResp);
  Sleep(2000);
  DeleteFile(ArqSTS);
  ArquivoReq.Free;
end;

Function TACBrTEFDBanese.CNC(Rede, NSU : String;
   DataHoraTransacao : TDateTime; Valor : Double) : Boolean;
begin
  {Não existe CNC}
  Result := True;
end;

Procedure TACBrTEFDBanese.NCN(Rede, NSU, Finalizacao : String;
   Valor : Double; DocumentoVinculado : String) ;
begin
  {Não existe NCN}
  //
end;

Function TACBrTEFDBanese.FazerRequisicao(AHeader : AnsiString = '';
  Valor : Double = 0; IndiceFPG_ECF : String = '') : Boolean ;
Var
  aNSU : AnsiString;
  ArquivoResposta : TStringList;
  ItemSelecionado : integer;
begin
  Result   := False ;

  ApagaEVerifica( ArqTemp );  // Apagando Arquivo Temporario anterior //
  ApagaEVerifica( ArqReq );   // Apagando Arquivo de Requisicao anterior //
  ApagaEVerifica( ArqSTS );   // Apagando Arquivo de Status anterior //
  ApagaEVerifica( ArqResp );  // Apagando Arquivo de Resposta anterior //

  if fpAguardandoResposta then
    raise EACBrTEFDErro.Create( ACBrStr( 'Requisição anterior não concluida' ) ) ;

  {Cria um NSU}
  aNSU := FormatDateTime('HHNNSS', Now );;

  if AHeader = 'ADM' then
    begin
      ItemSelecionado := -1 ;
      {Chama o evento e aguarda a resposta do item selecionado
       1 - Reimpressão
       2 - Cancelamento
       3 - Fechamento
      }
      fOnObtemInformacao( ItemSelecionado ) ;
      Result := True;
    end;

  if AHeader = 'CRT' then
    begin
      //ApagaEVerifica( ArqRespBkp );  // Apagando Arquivo de Backup da Resposta anterior //
      Result := RealizaTransacao(operCRT, Valor);
    end;

  if ((AHeader = 'ADM') and (ItemSelecionado = 1)) then
    begin
      {Verifica se o Arquivo de Backup da Resposta anterior existe}    
      if FileExists(ArqRespBkp) then
        Result := True
      else
        TACBrTEFD(Owner).DoExibeMsg( opmOK, 'Não existe arquivo de resposta para imprimir');
      end;
  if ((AHeader = 'ADM') and (ItemSelecionado = 2)) then
    begin
      {Apagando o Arquivo de Backup da Resposta anterior}
      //ApagaEVerifica( ArqRespBkp );
      Result := RealizaTransacao(operCancelamento, 0);
    end;

  if ((AHeader = 'ADM') and (ItemSelecionado = 3)) then
    begin
      Result := RealizaTransacao(operFechamento, 0);
    end;

  if not Result then
    Exit
     //raise EACBrTEFDErro.Create( ACBrStr( Erro ) )
  else
    begin
      Resp.Clear;

      with TACBrTEFDRespBanese( Resp ) do
      begin
        ArquivoResposta := TStringList.Create;

        if ((AHeader = 'ADM') and (ItemSelecionado = 1)) then
          begin
            ArquivoResposta.LoadFromFile(ArqRespBkp);
            MontaArquivoResposta('0', ArquivoResposta, operReimpressao);
            ImprimirComprovantes(ArquivoResposta);
          end
        else
          begin
            if ((AHeader = 'ADM') and (ItemSelecionado = 3)) then
              ArquivoResposta.LoadFromFile(ArqRespMovBkp)
            else
              ArquivoResposta.LoadFromFile(ArqRespBkp);

            if ((AHeader = 'ADM') and ((ItemSelecionado = 2) or (ItemSelecionado = 3))) then
              begin
                {Lê o Arquivo de Backup da Resposta anterior existe e monta uma
                estrutura que possa ser utilizada com as funções de impressão
                do ACBr}
                MontaArquivoResposta('0', ArquivoResposta, OperCancelamento);
                ImprimirComprovantes(ArquivoResposta);
              end
            else
              begin
                {Lê o Arquivo de Backup da Resposta anterior existe e monta uma
                estrutura que possa ser utilizada com as rotinas de impressão
                do ACBr}              
                MontaArquivoResposta(aNSU, ArquivoResposta, operCRT);

                {Grava informações utilizadas pelas rotinas de impressão do ACBr}
                Conteudo.Conteudo.Text := ArquivoResposta.Text;
                Conteudo.GravaInformacao(899,100, AHeader ) ;
                Conteudo.GravaInformacao(899,101, IntToStr(fpIDSeq) ) ;
                Conteudo.GravaInformacao(899,102, IndiceFPG_ECF ) ;
                Conteudo.GravaInformacao(899,103, IntToStr(Trunc(SimpleRoundTo( Valor * 100 ,0))) );
                Conteudo.GravaInformacao(899,104, AHeader );
                Conteudo.GravaInformacao(899,130, 'IMPRIMINDO...' ) ;

                Resp.TipoGP := fpTipo;
              end;
          end;
      end;
    end;
end;

function TACBrTEFDBanese.ProcessaRespostaRequisicao(operacao: TOperacaoTEFBanese) : Boolean;
var
   TempoInicioEspera : Double;
   Interromper, OK   : Boolean;
   RespostaRequisicao : TStringList;
begin
  Result := False;
  Interromper := False ;
  OK          := False ;
  try
     while not (OK or Interromper) do
     begin
        TempoInicioEspera := now ;
        fpAguardandoResposta := True ;
        try
           {Aguarda o arquivo de status}
           repeat
              Sleep( TACBrTEFD(Owner).EsperaSleep );  // Necessário Para não sobrecarregar a CPU //
  
              with TACBrTEFD(Owner) do
              begin
                 if Assigned( OnAguardaResp ) then
                    OnAguardaResp( ArqSTS, SecondsBetween(TempoInicioEspera, Now),
                                   Interromper ) ;
              end ;
           until FileExists( ArqSTS ) or Interromper ;

           DeleteFile(ArqSTS);
        finally
           fpAguardandoResposta := False ;
           with TACBrTEFD(Owner) do
           begin
              if Assigned( OnAguardaResp ) then
                 OnAguardaResp( ArqSTS, -1, Interromper ) ;
           end ;
        end;

        TempoInicioEspera := now ;
        try
           {Aguarda o arquivo de resposta}        
           repeat
              Sleep( TACBrTEFD(Owner).EsperaSleep );  // Necessário Para não sobrecarregar a CPU //

              with TACBrTEFD(Owner) do
              begin
                 if Assigned( OnAguardaResp ) then
                    OnAguardaResp( ArqResp, SecondsBetween(TempoInicioEspera, Now),
                                   Interromper ) ;
              end ;
           until FileExists( ArqResp ) or Interromper ;
        finally
           fpAguardandoResposta := False ;
           with TACBrTEFD(Owner) do
           begin
              if Assigned( OnAguardaResp ) then
                 OnAguardaResp( ArqResp, -1, Interromper ) ;
           end ;
        end;

        RespostaRequisicao := TStringList.Create;
        RespostaRequisicao.LoadFromFile(ArqResp);

        if copy(RespostaRequisicao[0],19,2) <> '00' then
          begin
             DeleteFile(ArqResp);
             TACBrTEFD(Owner).DoExibeMsg( opmOK, copy(RespostaRequisicao[0],25,length(RespostaRequisicao[0])));
             RespostaRequisicao.Free;
             Interromper := True;
          end
        else
          begin
            if (operacao = operFechamento) then
              RespostaRequisicao.SaveToFile(ArqRespMovBkp)
            else
              RespostaRequisicao.SaveToFile(ArqRespBkp);

            RespostaRequisicao.Free;
            DeleteFile(ArqSTS);
            DeleteFile(ArqResp);

            {Se for CRT ou cancelamento, envia a confirmação}
            if ((operacao = operCRT) or (operacao = operCancelamento))  then
              CNF('', '', '');

            OK := True;
            Result := True;
          end;
     end ;
  finally
    DeleteFile( ArqReq );
  end ;
end;

function TACBrTEFDBanese.ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean;
var
  ImpressaoOk : Boolean;
  RespostaPendente : TACBrTEFDResp ;
begin
  Result := True ;

  with TACBrTEFD(Owner) do
  begin
     Self.Resp.IndiceFPG_ECF := IndiceFPG_ECF;

     { Cria Arquivo de Backup, contendo Todas as Respostas }
     CopiarResposta ;

     { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
     RespostaPendente := TACBrTEFDRespBanese.Create ;
     RespostaPendente.Assign( Resp );
     RespostasPendentes.Add( RespostaPendente );

     if AutoEfetuarPagamento then
     begin
        ImpressaoOk := False ;

        try
           while not ImpressaoOk do
           begin
              try
                 ECFPagamento( IndiceFPG_ECF, Valor );
                 RespostasPendentes.SaldoAPagar  := RoundTo( RespostasPendentes.SaldoAPagar - Valor, -2 ) ;
                 RespostaPendente.OrdemPagamento := RespostasPendentes.Count + 1 ;
                 ImpressaoOk := True ;
              except
                 on EACBrTEFDECF do ImpressaoOk := False ;
                 else
                    raise ;
              end;

              if not ImpressaoOk then
              begin
                 if DoExibeMsg( opmYesNo, 'Impressora não responde'+sLineBreak+
                                          'Deseja imprimir novamente ?') <> mrYes then
                 begin
                    try ComandarECF(opeCancelaCupom); except {Exceção Muda} end ;
                    break ;
                 end;
              end;
           end;
        finally
           if not ImpressaoOk then
              CancelarTransacoesPendentes;
        end;
     end;

     if RespostasPendentes.SaldoRestante <= 0 then
     begin
        if AutoFinalizarCupom then
        begin
           FinalizarCupom( False );  { False não desbloqueia o MouseTeclado }
           ImprimirTransacoesPendentes;
        end;
     end ;
  end;
end;

procedure TACBrTEFDBanese.MontaArquivoResposta(aNSU: String; aRetorno:TStringList;
    operacao:TOperacaoTEFBanese);
var aResposta, imgCupom : TStringList;
    posicao : Integer;
begin
  aResposta := TStringList.Create;
  imgCupom := TStringList.Create;

  posicao := 25;
  while posicao < Length(aRetorno[0]) do
    begin
      imgCupom.Add(copy(aRetorno[0], posicao, 40));
      posicao := posicao + 40;
    end;

  if operacao = operCRT then
    begin
      aResposta.Add('134-000 = ' + aNSU);
      aResposta.Add('102-000 = T.E.F.');
      aResposta.Add('121-000 = ' +  BinaryStringToString(imgCupom.Text));
    end
  else
    aResposta.Text := imgCupom.Text;

  aRetorno.Text := aResposta.Text;
  aResposta.Clear;
end;

procedure TACBrTEFDBanese.ImprimirComprovantes(SL : TStringList);
var
  ImpressaoOk, FechaGerencialAberto, GerencialAberto : Boolean;
  Est : AnsiChar;
  TempoInicio : TDateTime;
begin
  CopiarResposta;

  GerencialAberto      := False;
  ImpressaoOk          := False ;
  FechaGerencialAberto := False ;
  TempoInicio          := now ;
  
  with TACBrTEFD(Owner) do
  begin
    try
      BloquearMouseTeclado( True );

      while not ImpressaoOk do
      begin
        try
          try
           if FechaGerencialAberto then
           begin
             Est := EstadoECF;

             { Fecha Vinculado ou Gerencial ou Cupom, se ficou algum aberto por Desligamento }
             case Est of
               'C'      : ComandarECF( opeFechaVinculado );
               'G', 'R' : ComandarECF( opeFechaGerencial );
               'V', 'P' : ComandarECF( opeCancelaCupom );
             end;

             GerencialAberto      := False ;
             FechaGerencialAberto := False ;

             if EstadoECF <> 'L' then
               raise EACBrTEFDECF.Create( ACBrStr('ECF não está LIVRE') ) ;
           end;

            TempoInicio     := now ;
            DoExibeMsg( opmExibirMsgOperador, 'IMPRIMINDO...' ) ;

            if SL.Text <> '' then
            begin
              if not GerencialAberto then
               begin
                 ComandarECF( opeAbreGerencial ) ;
                 GerencialAberto := True ;
               end;

              ECFImprimeVia( trGerencial, 1, SL );

              ImpressaoOk := True ;
            end;

          if GerencialAberto then
          begin
             ComandarECF( opeFechaGerencial );
             GerencialAberto := False;
          end;
          finally
            { Verifica se Mensagem Ficou pelo menos por 5 segundos }
            if ImpressaoOk then
            begin
              while SecondsBetween(now,TempoInicio) < 5 do
              begin
                Sleep(EsperaSleep);
                {$IFNDEF NOGUI}
                Application.ProcessMessages;
                {$ENDIF}
              end;
            end;

            DoExibeMsg( opmRemoverMsgOperador, '' ) ;
          end;
        except
          on EACBrTEFDECF do ImpressaoOk := False ;
          else
             raise ;
        end;

        if not ImpressaoOk then
        begin
          if DoExibeMsg( opmYesNo, 'Impressora não responde'+sLineBreak+
                                   'Deseja imprimir novamente ?') <> mrYes then
            break ;

          FechaGerencialAberto := True ;
        end;
      end ;
    finally
      SL.Free;
    end;
  end;
end;

function TACBrTEFDBanese.RealizaTransacao(operacao : TOperacaoTEFBanese; Valor : Double): Boolean;
var
  ArquivoRequisicao : TStringList;
begin
  ArquivoRequisicao := TStringList.Create;
  try
    case operacao of
      operCRT          : ArquivoRequisicao.Add('SP0001TTTTC' +
                         PadLeft(RemoveString(',', FormatFloat('0.00', Valor)), 12, '0'));
      operCancelamento : ArquivoRequisicao.Add('SP0001TTTTL');
      operFechamento   : ArquivoRequisicao.Add('SP0001TTTTM')
    end;
    ArquivoRequisicao.SaveToFile('C:\bcard\req\pergunta.tmp');
  finally
    ArquivoRequisicao.Free;
  end ;

  RenameFile(fArqTmp, fArqReq);

  Result := ProcessaRespostaRequisicao(operacao);
end;

procedure TACBrTEFDBanese.SetArqRespMovBkp(const Value: String);
begin
  fArqRespMovBkp := Value;
end;

end.

