package com.acbr.pixcd.acbrlibpixcd.demo.configuracoes.psp;

import android.Manifest;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ConfiguracoesAilosFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXAilos;
    private EditText txtClientIDAilos;
    private EditText txtClientSecretAilos;
    private EditText txtArquivoChavePrivadaAilos;
    private Button btnArquivoChavePrivadaAilos;
    private EditText txtArquivoCertificadoAilos;
    private Button btnArquivoCertificadoAilos;
    private EditText txtArquivoCertificadoRootAilos;
    private Button btnArquivoCertificadoRootAilos;
    private EditText txtScopesAilos;
    private Button btnSalvarConfiguracoesAilos;
    private Button btnCarregarConfiguracoesAilos;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_ailos, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXAilos = view.findViewById(R.id.txtChavePIXAilos);
        txtClientIDAilos = view.findViewById(R.id.txtClientIDAilos);
        txtClientSecretAilos = view.findViewById(R.id.txtClientSecretAilos);
        txtArquivoChavePrivadaAilos = view.findViewById(R.id.txtArquivoChavePrivadaAilos);
        btnArquivoChavePrivadaAilos = view.findViewById(R.id.btnArquivoChavePrivadaAilos);
        txtArquivoCertificadoAilos = view.findViewById(R.id.txtArquivoCertificadoAilos);
        btnArquivoCertificadoAilos = view.findViewById(R.id.btnArquivoCertificadoAilos);
        txtArquivoCertificadoRootAilos = view.findViewById(R.id.txtArquivoCertificadoRootAilos);
        btnArquivoCertificadoRootAilos = view.findViewById(R.id.btnArquivoCertificadoRootAilos);
        txtScopesAilos = view.findViewById(R.id.txtScopesAilos);
        btnSalvarConfiguracoesAilos = view.findViewById(R.id.btnSalvarConfiguracoesAilos);
        btnCarregarConfiguracoesAilos = view.findViewById(R.id.btnCarregarConfiguracoesAilos);

        application = (PIXCDApplication) getActivity().getApplicationContext();

        btnArquivoChavePrivadaAilos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 2);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarChavePrivadaAilos();
                }
            }
        });

        btnArquivoCertificadoAilos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 3);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarArquivoCertificadoAilos();
                }
            }
        });

        btnArquivoCertificadoRootAilos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 4);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarArquivoCertificadoRootAilos();
                }
            }
        });

        btnSalvarConfiguracoesAilos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesAilos();
            }
        });

        btnCarregarConfiguracoesAilos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesAilos();
            }
        });

        carregarConfiguracoesAilos();

        return view;
    }

    private void carregarChavePrivadaAilos(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/octet-stream"};  // .key files
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo KEY"), 2);
    }

    private void carregarArquivoCertificadoAilos(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-x509-ca-cert", "application/x-pem-file", "text/plain"};
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo PEM"), 3);
    }

    private void carregarArquivoCertificadoRootAilos(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-x509-ca-cert", "application/pkix-cert"};
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo CRT"), 4);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == 2 || requestCode == 3 || requestCode == 4) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                if (requestCode == 2) {
                    carregarChavePrivadaAilos();
                } else if (requestCode == 3) {
                    carregarArquivoCertificadoAilos();
                } else if (requestCode == 4) {
                    carregarArquivoCertificadoRootAilos();
                }
            } else {
                Toast.makeText(getContext(), "Permissão necessária para acessar arquivos.", Toast.LENGTH_SHORT).show();
            }
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        if (resultCode == Activity.RESULT_OK && data != null) {
            Uri uri = data.getData();
            if (uri != null) {
                try {
                    String savedPath = null;

                    if (requestCode == 2) {
                        savedPath = saveFileFromUri(uri, "certificado.key");
                        txtArquivoChavePrivadaAilos.setText(savedPath);
                        Toast.makeText(getContext(), "Chave privada carregada com sucesso!", Toast.LENGTH_SHORT).show();
                    } else if (requestCode == 3) {
                        savedPath = saveFileFromUri(uri, "certificado.pem");
                        txtArquivoCertificadoAilos.setText(savedPath);
                        Toast.makeText(getContext(), "Certificado público carregado com sucesso!", Toast.LENGTH_SHORT).show();
                    } else if (requestCode == 4) {
                        savedPath = saveFileFromUri(uri, "certificado.crt");
                        txtArquivoCertificadoRootAilos.setText(savedPath);
                        Toast.makeText(getContext(), "Certificado CRT carregado com sucesso!", Toast.LENGTH_SHORT).show();
                    }

                    if (savedPath == null) {
                        Toast.makeText(getContext(), "Erro ao salvar o arquivo.", Toast.LENGTH_SHORT).show();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                    Toast.makeText(getContext(), "Erro ao processar o arquivo: " + e.getMessage(), Toast.LENGTH_SHORT).show();
                }
            }
        }
    }

    private String saveFileFromUri(Uri uri, String fileName) throws IOException {
        // Diretório interno do aplicativo
        File directory = new File(getContext().getFilesDir(), "certificados");
        if (!directory.exists()) {
            directory.mkdirs();
        }

        File file = new File(directory, fileName);

        try (InputStream inputStream = getContext().getContentResolver().openInputStream(uri);
             FileOutputStream outputStream = new FileOutputStream(file)) {

            byte[] buffer = new byte[1024];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
        }
        // Retorna o caminho absoluto do arquivo salvo
        return file.getAbsolutePath();
    }

    private void salvarConfiguracoesAilos(){
        try{
            ACBrPIXCD.configGravarValor("Ailos", "ChavePIX", txtChavePIXAilos.getText().toString());
            ACBrPIXCD.configGravarValor("Ailos", "ClientID", txtClientIDAilos.getText().toString());
            ACBrPIXCD.configGravarValor("Ailos", "ClientSecret", txtClientSecretAilos.getText().toString());
            ACBrPIXCD.configGravarValor("Ailos", "ArqChavePrivada", txtArquivoChavePrivadaAilos.getText().toString());
            ACBrPIXCD.configGravarValor("Ailos", "ArqCertificado", txtArquivoCertificadoAilos.getText().toString());
            ACBrPIXCD.configGravarValor("Ailos", "ArqCertificadoRoot", txtArquivoCertificadoRootAilos.getText().toString());
            ACBrPIXCD.configGravarValor("Ailos", "Scopes", txtScopesAilos.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Ailos: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesAilos(){
        try{
            txtChavePIXAilos.setText(ACBrPIXCD.configLerValor("Ailos", "ChavePIX"));
            txtClientIDAilos.setText(ACBrPIXCD.configLerValor("Ailos", "ClientID"));
            txtClientSecretAilos.setText(ACBrPIXCD.configLerValor("Ailos", "ClientSecret"));
            txtArquivoChavePrivadaAilos.setText(ACBrPIXCD.configLerValor("Ailos", "ArqChavePrivada"));
            txtArquivoCertificadoAilos.setText(ACBrPIXCD.configLerValor("Ailos", "ArqCertificado"));
            txtArquivoCertificadoRootAilos.setText(ACBrPIXCD.configLerValor("Ailos", "ArqCertificadoRoot"));
            txtScopesAilos.setText(ACBrPIXCD.configLerValor("Ailos", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Ailos: " + ex.getMessage());
        }
    }
}