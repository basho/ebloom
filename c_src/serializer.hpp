/*
 **************************************************************************
 *                                                                        *
 *                     Binary Data Serializer Library                     *
 *                                                                        *
 * Author: Arash Partow - 2001                                            *
 * URL: http://www.partow.net                                             *
 *                                                                        *
 * Copyright notice:                                                      *
 * Free use of the Binary Data Serializer Library is permitted under the  *
 * guidelines and in accordance with the most current version of the      *
 * Common Public License.                                                 *
 * http://www.opensource.org/licenses/cpl.php                             *
 *                                                                        *
 **************************************************************************
*/

#ifndef INCLUDE_SERIALIZER_HPP
#define INCLUDE_SERIALIZER_HPP


#include <iostream>
#include <string>
#include <algorithm>
#include <fstream>


class serializer
{
public:

   serializer(char* buffer, const unsigned int& buffer_length)
   : original_buffer_(buffer),
     buffer_(buffer + 1),
     buffer_length_(buffer_length)
   {
   }

   void reset()
   {
      written_buffer_size_ = 0;
      read_buffer_size_    = 0;
      buffer_ = original_buffer_;
   }

   void clear()
   {
      reset();
      for(unsigned int i = 0; i < buffer_length_; ++i)
      {
         buffer_[i] = 0;
      }
   }

   void delete_internal_buffer()
   {
      delete[] original_buffer_;
      original_buffer_ = 0;
      buffer_ = 0;
   }

   unsigned int length() { return written_buffer_size_; }

   template<typename T> inline bool operator >> (T& output) { return read (output); }
   template<typename T> inline bool operator << (T& output) { return write(output); }

   inline bool read(char& output)         { return read_pod(output); }
   inline bool read(int& output)          { return read_pod(output); }
   inline bool read(unsigned int& output) { return read_pod(output); }
   inline bool read(float& output)        { return read_pod(output); }
   inline bool read(double& output)       { return read_pod(output); }
   inline bool read(long unsigned int& output) { return read_pod(output); }
   inline bool read(unsigned char& output) { return read_pod(output); }

   inline bool read(std::string& output)
   {
      unsigned int length = 0;
      if (!read_pod(length))
      {
         return false;
      }

      if ((length + sizeof(unsigned int) + read_buffer_size_) > buffer_length_)
      {
         return false;
      }
      output.resize(length);
      std::copy(buffer_, buffer_ + length,output.begin());
      buffer_ += length;
      read_buffer_size_ += length;
      return true;
   }

   inline bool write(const char&         input) { return write_pod(input); }
   inline bool write(const int&          input) { return write_pod(input); }
   inline bool write(const unsigned int& input) { return write_pod(input); }
   inline bool write(const float&        input) { return write_pod(input); }
   inline bool write(const double&       input) { return write_pod(input); }
   inline bool write(const short&        input) { return write_pod(input); }
   inline bool write(const long unsigned int& input) { return write_pod(input); }
   inline bool write(const unsigned char& input) { return write_pod(input); }
   
   inline bool write(std::string& input)
   {
      return write(input.c_str(),static_cast<unsigned int>(input.size()));
   }

   inline bool write(const char* data, unsigned int length)
   {
      if ((length + sizeof(unsigned int) + written_buffer_size_) > buffer_length_)
      {
         return false;
      }
      write(length);
      std::copy(data,data + length,buffer_);
      buffer_ += length;
      written_buffer_size_ += length;
      return true;
   }

   inline void write_to_stream(std::ofstream& stream)
   {
      stream.write(original_buffer_,written_buffer_size_);
   }

   inline void read_from_stream(std::ifstream& stream, const unsigned int length)
   {
      if (length > buffer_length_)
      {
         return;
      }
      stream.read(original_buffer_,length);
   }

   inline void write_to_buffer(char data[])
   {
      std::copy(original_buffer_,original_buffer_ + written_buffer_size_,data);
   }

   inline void read_from_buffer(const char data[], const unsigned int& length)
   {
      if (length > buffer_length_)
      {
         return;
      }
      std::copy(data,data + length,original_buffer_);
   }

private:

   serializer();
   serializer(const serializer& s);
   serializer& operator=(const serializer& s);

   template<typename T>
   inline bool write_pod(const T& data)
   {
      const unsigned int data_length = sizeof(T);
      if ((data_length + written_buffer_size_) > buffer_length_)
      {
         return false;
      }
      const char* ptr = reinterpret_cast<const char*>(&data);
      const char* end = reinterpret_cast<const char*>(&data) + sizeof(T);
      for(; ptr != end; ++buffer_, ++ptr)
      {
         *buffer_ = *ptr;
      }
      written_buffer_size_ += data_length;
      return true;
   }

   template<typename T>
   inline bool read_pod(T& data)
   {
      const unsigned int data_length = sizeof(T);
      if ((data_length + read_buffer_size_) > buffer_length_)
      {
         return false;
      }
      char* ptr = reinterpret_cast<char*>(&data);
      char* end = reinterpret_cast<char*>(&data) + sizeof(T);
      for(; ptr != end; ++buffer_, ++ptr)
      {
         *ptr = *buffer_;
      }
      read_buffer_size_ += data_length;
      return true;
   }

   char* original_buffer_;
   char* buffer_;
   unsigned int buffer_length_;
   unsigned int written_buffer_size_;
   unsigned int read_buffer_size_;
};

#endif

