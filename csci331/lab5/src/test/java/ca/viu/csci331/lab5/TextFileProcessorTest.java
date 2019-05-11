package ca.viu.csci331.lab5;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;


public class TextFileProcessorTest {
    private TextFileProcessor textFileProcessor;
    
    
    @Before
    public void prepareTextFileProcessor() {
        textFileProcessor = new TextFileProcessor("./src/test/resources/original.txt", "./src/test/resources/processed.txt");
    }
    
    
    @Rule
    public ExpectedException expectedException = ExpectedException.none();
    
    
    @Test
    public void testOpenInputStream() throws IOException {

        textFileProcessor.openInputStream();
        assertTrue(textFileProcessor.getInputStream().available() > 0);
    }
    
    
    @Test
    public void testCloseInputStream() throws IOException {
        
        textFileProcessor.openInputStream();
        assertTrue(textFileProcessor.getInputStream().available() > 0);
        
        textFileProcessor.closeInputStream();
        expectedException.expect(IOException.class);
        textFileProcessor.getInputStream().available();
    }
    
    
    @Test
    public void testOpenOutputStream() throws IOException {
        byte[] myByteArray = "A".getBytes();
        textFileProcessor.openOutputStream();
        textFileProcessor.getOutputStream().write(myByteArray);
    }
    
    @Test
    public void testCloseOutputStream() throws IOException {
        byte[] myByteArray = "A".getBytes();
        
        OutputStream myFileOutputStream = new FileOutputStream("./src/test/resources/processed.txt");
        textFileProcessor.setOutputStream(myFileOutputStream);
        myFileOutputStream.write(myByteArray);
        
        textFileProcessor.closeOutputStream();
        expectedException.expect(IOException.class);
        myFileOutputStream.write(myByteArray);
    }
    
    
    @Test
    public void testRead() throws IOException {
        String expected = "The English edition of Wikipedia has grown to 5,802,460 articles, equivalent to over 2,000 print volumes of the Encyclopedia Britannica. Including all language editions, Wikipedia has over 38 million articles, equivalent to over 15,000 print volumes. Wikipedia was founded as an offshoot of Nupedia, a now-abandoned project to produce a free encyclopedia, begun by the online media company Bomis. Nupedia had an elaborate system of peer review and required highly qualified contributors, but the writing of articles was slow. During 2000, Jimmy Wales (founder of Nupedia and co-founder of Bomis), and Larry Sanger, whom Wales had employed to work on the encyclopedia project, discussed ways of supplementing Nupedia with a more open, complementary project. Multiple sources suggested that a wiki might allow members of the public to contribute material, and Nupedia's first wiki went online on January 10, 2001. There was considerable resistance on the part of Nupedia's editors and reviewers to the idea of associating Nupedia with a website in the wiki format, so the new project was given the name \"Wikipedia\" and launched on its own domain, wikipedia.com, on January 15 (now called \"Wikipedia Day\" by some users). The bandwidth and server (in San Diego) were donated by Wales. Other current and past Bomis employees who have worked on the project include Tim Shell, one of the cofounders of Bomis and its current CEO, and programmer Jason Richey.In May 2001, a large number of non-English Wikipedias were launched�in Catalan, Chinese, Dutch, Esperanto, French, German, Hebrew, Italian, Japanese, Portuguese, Russian, Spanish, and Swedish. These were soon joined by Arabic and Hungarian. In September,[2] Polish was added, and further commitment to the multilingual provision of Wikipedia was made. At the end of the year, Afrikaans, Norwegian, and Serbo-Croatian versions were announced. The domain was eventually changed to the present wikipedia.org when the not-for-profit Wikimedia Foundation was launched, in 2003, as its new parent organization, with the \"org\" top-level domain denoting its non-commercial nature. Today, there are Wikipedias in over 250 languages.";
        textFileProcessor.setInputStream(new FileInputStream(textFileProcessor.getInputFileName()));
        assertEquals(expected, textFileProcessor.read());
    }
    
    
    @Test
    public void testReadUnopenedOutputStream() throws IOException {
        expectedException.expect(IOException.class);
        textFileProcessor.read();
    }
    
    
    @Test
    public void testWrite() throws IOException {
        String expected = "The English edition of Wikipedia has grown to 5,802,460 articles, equivalent to over 2,000 print volumes of the Encyclopedia Britannica. Including all language editions, Wikipedia has over 38 million articles, equivalent to over 15,000 print volumes. Wikipedia was founded as an offshoot of Nupedia, a now-abandoned project to produce a free encyclopedia, begun by the online media company Bomis. Nupedia had an elaborate system of peer review and required highly qualified contributors, but the writing of articles was slow. During 2000, Jimmy Wales (founder of Nupedia and co-founder of Bomis), and Larry Sanger, whom Wales had employed to work on the encyclopedia project, discussed ways of supplementing Nupedia with a more open, complementary project. Multiple sources suggested that a wiki might allow members of the public to contribute material, and Nupedia's first wiki went online on January 10, 2001. There was considerable resistance on the part of Nupedia's editors and reviewers to the idea of associating Nupedia with a website in the wiki format, so the new project was given the name \"Wikipedia\" and launched on its own domain, wikipedia.com, on January 15 (now called \"Wikipedia Day\" by some users). The bandwidth and server (in San Diego) were donated by Wales. Other current and past Bomis employees who have worked on the project include Tim Shell, one of the cofounders of Bomis and its current CEO, and programmer Jason Richey.In May 2001, a large number of non-English Wikipedias were launched�in Catalan, Chinese, Dutch, Esperanto, French, German, Hebrew, Italian, Japanese, Portuguese, Russian, Spanish, and Swedish. These were soon joined by Arabic and Hungarian. In September,[2] Polish was added, and further commitment to the multilingual provision of Wikipedia was made. At the end of the year, Afrikaans, Norwegian, and Serbo-Croatian versions were announced. The domain was eventually changed to the present wikipedia.org when the not-for-profit Wikimedia Foundation was launched, in 2003, as its new parent organization, with the \"org\" top-level domain denoting its non-commercial nature. Today, there are Wikipedias in over 250 languages.";
        textFileProcessor.setOutputStream(new FileOutputStream(textFileProcessor.getOutputFileName()));
        
        textFileProcessor.write(expected);
        textFileProcessor.getOutputStream().close();
        
        StringBuilder stringBuilder = new StringBuilder();
        
        try {
            Reader reader = new InputStreamReader(new FileInputStream(textFileProcessor.getOutputFileName()), "UTF-8");
            int ch = reader.read();
            while(ch >= 0) {
                stringBuilder.append((char)ch);
                ch = reader.read();
            }
            reader.close();
        } catch (NullPointerException e) {
            throw new IOException();
        }

        assertEquals(expected, stringBuilder.toString());
    }
    
    
    @Test
    public void testWriteUnopenedOutputStream() throws IOException {
        expectedException.expect(IOException.class);
        textFileProcessor.write("test");
    }
    
    
    @Test
    public void testProcess() {
        String unprocessed = "The English edition of Wikipedia has grown to 5,802,460 articles, equivalent to over 2,000 print volumes of the Encyclopedia Britannica. Including all language editions, Wikipedia has over 38 million articles, equivalent to over 15,000 print volumes. Wikipedia was founded as an offshoot of Nupedia, a now-abandoned project to produce a free encyclopedia, begun by the online media company Bomis. Nupedia had an elaborate system of peer review and required highly qualified contributors, but the writing of articles was slow. During 2000, Jimmy Wales (founder of Nupedia and co-founder of Bomis), and Larry Sanger, whom Wales had employed to work on the encyclopedia project, discussed ways of supplementing Nupedia with a more open, complementary project. Multiple sources suggested that a wiki might allow members of the public to contribute material, and Nupedia's first wiki went online on January 10, 2001. There was considerable resistance on the part of Nupedia's editors and reviewers to the idea of associating Nupedia with a website in the wiki format, so the new project was given the name \"Wikipedia\" and launched on its own domain, wikipedia.com, on January 15 (now called \"Wikipedia Day\" by some users). The bandwidth and server (in San Diego) were donated by Wales. Other current and past Bomis employees who have worked on the project include Tim Shell, one of the cofounders of Bomis and its current CEO, and programmer Jason Richey.In May 2001, a large number of non-English Wikipedias were launched�in Catalan, Chinese, Dutch, Esperanto, French, German, Hebrew, Italian, Japanese, Portuguese, Russian, Spanish, and Swedish. These were soon joined by Arabic and Hungarian. In September,[2] Polish was added, and further commitment to the multilingual provision of Wikipedia was made. At the end of the year, Afrikaans, Norwegian, and Serbo-Croatian versions were announced. The domain was eventually changed to the present wikipedia.org when the not-for-profit Wikimedia Foundation was launched, in 2003, as its new parent organization, with the \"org\" top-level domain denoting its non-commercial nature. Today, there are Wikipedias in over 250 languages.";
        String expected = "Sentence-1, Word Count-20\nThe English edition of Wikipedia has grown to 5,802,460 articles, equivalent to over 2,000 print volumes of the Encyclopedia Britannica.\n\nSentence-2, Word Count-16\nIncluding all language editions, Wikipedia has over 38 million articles, equivalent to over 15,000 print volumes.\n\nSentence-3, Word Count-23\nWikipedia was founded as an offshoot of Nupedia, a now-abandoned project to produce a free encyclopedia, begun by the online media company Bomis.\n\nSentence-4, Word Count-20\nNupedia had an elaborate system of peer review and required highly qualified contributors, but the writing of articles was slow.\n\nSentence-5, Word Count-35\nDuring 2000, Jimmy Wales (founder of Nupedia and co-founder of Bomis), and Larry Sanger, whom Wales had employed to work on the encyclopedia project, discussed ways of supplementing Nupedia with a more open, complementary project.\n\nSentence-6, Word Count-25\nMultiple sources suggested that a wiki might allow members of the public to contribute material, and Nupedia's first wiki went online on January 10, 2001.\n\nSentence-7, Word Count-41\nThere was considerable resistance on the part of Nupedia's editors and reviewers to the idea of associating Nupedia with a website in the wiki format, so the new project was given the name \"Wikipedia\" and launched on its own domain, wikipedia.\n\nSentence-8, Word Count-11\ncom, on January 15 (now called \"Wikipedia Day\" by some users).\n\nSentence-9, Word Count-11\nThe bandwidth and server (in San Diego) were donated by Wales.\n\nSentence-10, Word Count-29\nOther current and past Bomis employees who have worked on the project include Tim Shell, one of the cofounders of Bomis and its current CEO, and programmer Jason Richey.\n\nSentence-11, Word Count-25\nIn May 2001, a large number of non-English Wikipedias were launched�in Catalan, Chinese, Dutch, Esperanto, French, German, Hebrew, Italian, Japanese, Portuguese, Russian, Spanish, and Swedish.\n\nSentence-12, Word Count-8\nThese were soon joined by Arabic and Hungarian.\n\nSentence-13, Word Count-16\nIn September,[2] Polish was added, and further commitment to the multilingual provision of Wikipedia was made.\n\nSentence-14, Word Count-13\nAt the end of the year, Afrikaans, Norwegian, and Serbo-Croatian versions were announced.\n\nSentence-15, Word Count-9\nThe domain was eventually changed to the present wikipedia.\n\nSentence-16, Word Count-24\norg when the not-for-profit Wikimedia Foundation was launched, in 2003, as its new parent organization, with the \"org\" top-level domain denoting its non-commercial nature.\n\nSentence-17, Word Count-8\nToday, there are Wikipedias in over 250 languages.\n\nTotal Sentences: 17, Total Words: 334";
        
        assertEquals(expected, textFileProcessor.process(unprocessed));
        
    }
}
