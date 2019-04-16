unit ACBrLibBPeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrBPeLib }

  TTestACBrBPeLib = class(TTestCase)
  published
    procedure Test_BPe_Inicializar_Com_DiretorioInvalido;
    procedure Test_BPe_Inicializar;
    procedure Test_BPe_Inicializar_Ja_Inicializado;
    procedure Test_BPe_Finalizar;
    procedure Test_BPe_Finalizar_Ja_Finalizado;
    procedure Test_BPe_Nome_Obtendo_LenBuffer;
    procedure Test_BPe_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_BPe_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_BPe_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_BPe_Versao;
    procedure Test_BPe_ConfigLerValor;
    procedure Test_BPe_ConfigGravarValor;
    {
    procedure Test_BPe_StatusServico;

    procedure Test_BPe_LimparLista;
    procedure Test_BPe_CarregarXML;
    procedure Test_BPe_Imprimir;
    procedure Test_BPe_ImprimirPDF;

    procedure Test_BPe_CarregarINI;
    procedure Test_BPe_Assinar;
    procedure Test_BPe_Validar;
    procedure Test_BPe_ValidarRegrasdeNegocios;
    procedure Test_BPe_VerificarAssinatura;

    procedure Test_BPe_Enviar;
    procedure Test_BPe_Consultar;
    procedure Test_BPe_Cancelar;
    procedure Test_BPe_EnviarEmail;

    procedure Test_BPe_EnviarEvento;
    procedure Test_BPe_EnviarEmailEvento;
    procedure Test_BPe_ImprimirEvento;
    procedure Test_BPe_ImprimirEventoPDF;
    }
  end;

implementation

uses
  ACBrLibBPeStaticImport, ACBrLibBPeConsts, ACBrLibConsts;

procedure TTestACBrBPeLib.Test_BPe_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, BPe_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrBPeLib.Test_BPe_Inicializar;
begin
  AssertEquals(ErrOk, BPe_Inicializar('',''));
end;

procedure TTestACBrBPeLib.Test_BPe_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, BPe_Inicializar('',''));
end;

procedure TTestACBrBPeLib.Test_BPe_Finalizar;
begin
  AssertEquals(ErrOk, BPe_Finalizar());
end;

procedure TTestACBrBPeLib.Test_BPe_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, BPe_Finalizar());
end;

procedure TTestACBrBPeLib.Test_BPe_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, BPe_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibBPeNome), Bufflen);
end;

procedure TTestACBrBPeLib.Test_BPe_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibBPeNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BPe_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBPeNome), Bufflen);
  AssertEquals(CLibBPeNome, AStr);
end;

procedure TTestACBrBPeLib.Test_BPe_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibBPeNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BPe_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibBPeNome), Bufflen);
  AssertEquals(CLibBPeNome, AStr);
end;

procedure TTestACBrBPeLib.Test_BPe_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BPe_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibBPeNome,1,4), AStr);
end;

procedure TTestACBrBPeLib.Test_BPe_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, BPe_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibBPeVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BPe_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBPeVersao), Bufflen);
  AssertEquals(CLibBPeVersao, AStr);
end;

procedure TTestACBrBPeLib.Test_BPe_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BPe_ConfigLerValor(CSessaoVersao, CLibBPeNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibBPeVersao, AStr);
end;

procedure TTestACBrBPeLib.Test_BPe_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, BPe_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BPe_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;
(*
procedure TTestACBrBPeLib.Test_BPe_StatusServico;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a consulta ao Status de Serviço
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao consultar ao Status de Serviço', ErrOk, BPe_StatusServico(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrBPeLib.Test_BPe_LimparLista;
begin
  // Iniciando a Limpeza da Lista de BP-e
  AssertEquals('Erro ao limpar a lista de BP-e', ErrOk, BPe_LimparLista);
end;

procedure TTestACBrBPeLib.Test_BPe_CarregarXML;
begin
  {
  // Iniciando o Carregamento do XML do BP-e
  AssertEquals('Erro ao carregar o XML do BP-e', ErrOk,
   BPe_CarregarXML('C:\ERP\XML\201808\BPe\35180804550110000188570010000009491283342822-BPe.xml'));
  }
end;

procedure TTestACBrBPeLib.Test_BPe_Imprimir;
begin
  // Iniciando a Impressão do DABPe
//  AssertEquals('Erro ao Imprimir o DABPe', ErrOk, BPe_Imprimir);
end;

procedure TTestACBrBPeLib.Test_BPe_ImprimirPDF;
begin
  // Iniciando a geração do PDF do DABPe
//  AssertEquals('Erro ao gerar o PDF do DABPe', ErrOk, BPe_ImprimirPDF);
end;

procedure TTestACBrBPeLib.Test_BPe_CarregarINI;
begin
  Test_BPe_LimparLista;

  // Iniciando o Carregamento do INI do BP-e
  AssertEquals('Erro ao carregar o INI do BP-e', ErrOk,
   BPe_CarregarINI('C:\ACBr\trunk2\Projetos\ACBrLib\Testes\BPe\Modelo-BPe.ini'));
end;

procedure TTestACBrBPeLib.Test_BPe_Assinar;
begin
  // Iniciando a Assinatura
  AssertEquals('Erro ao Assinar', ErrOk, BPe_Assinar);
end;

procedure TTestACBrBPeLib.Test_BPe_Validar;
begin
  // Iniciando a Validação
  AssertEquals('Erro ao Validar', ErrOk, BPe_Validar);
end;

procedure TTestACBrBPeLib.Test_BPe_ValidarRegrasdeNegocios;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Validação de Regras de Negócios
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Validar regras de negócio', ErrOk,
    BPe_ValidarRegrasdeNegocios(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrBPeLib.Test_BPe_VerificarAssinatura;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Verificação de Assinatura
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Verificar assinatura', ErrOk,
    BPe_VerificarAssinatura(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrBPeLib.Test_BPe_Enviar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  {
  // Iniciando o Envio do Lote de BP-e
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Enviar Lote de BP-e', ErrOk,
    BPe_Enviar(1, True, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  }
end;

procedure TTestACBrBPeLib.Test_BPe_Consultar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  Test_BPe_LimparLista;
  {
  // Iniciando a Consulta
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Consultar o BP-e', ErrOk,
    BPe_Consultar('C:\ERP\XML\201808\BPe\35180804550110000188570010000009491283342822-BPe.xml', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  }
end;

procedure TTestACBrBPeLib.Test_BPe_Cancelar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  Test_BPe_LimparLista;
  {
  // Iniciando o Cancelamento
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Cancelar o BP-e', ErrOk,
    BPe_Cancelar('35180804550110000188570010000009491283342822',
                 'Desacordo comercial', '04550110000188', 1, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  }
end;

procedure TTestACBrBPeLib.Test_BPe_EnviarEmail;
begin
  //a
end;

procedure TTestACBrBPeLib.Test_BPe_EnviarEvento;
begin
  //a
end;

procedure TTestACBrBPeLib.Test_BPe_EnviarEmailEvento;
var
  Path, ArqBPe, ArqEvento: String;
begin
  {
  // Iniciando o envio do evento por email
  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\BPe\bin\';
  ArqBPe := Path + '28140417957142000144570170000000311556600342-BPe.xml';
  ArqEvento := Path + '2814041795714200014457017000000031155660034211011001-procEventoBPe.xml';

  AssertEquals('Erro ao enviar email do evento', ErrOk,
    BPe_EnviarEmailEvento('italo.jurisato@gmail.com', PChar(ArqEvento),
      PChar(ArqBPe), True, 'Evento', '', '',
      'Teste de envio de evento por email.'));
  }
end;

procedure TTestACBrBPeLib.Test_BPe_ImprimirEvento;
var
  Path, ArqBPe, ArqEvento: String;
begin
  {
  // Iniciando a Impressão do Evento
  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\BPe\bin\';
  ArqBPe := Path + '28140417957142000144570170000000311556600342-BPe.xml';
  ArqEvento := Path + '2814041795714200014457017000000031155660034211011001-procEventoBPe.xml';

  AssertEquals('Erro ao imprimir o evento', ErrOk,
    BPe_ImprimirEvento(PChar(ArqBPe), PChar(ArqEvento)));
  }
end;

procedure TTestACBrBPeLib.Test_BPe_ImprimirEventoPDF;
var
  Path, ArqBPe, ArqEvento: String;
begin
  {
  // Iniciando a geração do PDF do Evento
  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\BPe\bin\';
  ArqBPe := Path + '28140417957142000144570170000000311556600342-BPe.xml';
  ArqEvento := Path + '2814041795714200014457017000000031155660034211011001-procEventoBPe.xml';

  AssertEquals('Erro ao gerar o PDF do evento', ErrOk,
    BPe_ImprimirEventoPDF(PChar(ArqBPe), PChar(ArqEvento)));
  }
end;
*)
initialization
  RegisterTest(TTestACBrBPeLib);

end.

