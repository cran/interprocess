#include <cpp11.hpp>
#include <boost/interprocess/sync/named_semaphore.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

using namespace boost::interprocess;
namespace pt = boost::posix_time;


/*-------------------*/
/*  CREATE / REMOVE  */
/*-------------------*/


[[cpp11::register]]
void cpp_sem_create_only(std::string name, unsigned int initial_count) {
  named_semaphore sem(create_only_t(), name.c_str(), initial_count);
}

[[cpp11::register]]
void cpp_sem_open_only(std::string name) {
  named_semaphore sem(open_only_t(), name.c_str());
}

[[cpp11::register]]
void cpp_sem_open_create(std::string name, unsigned int initial_count) {
  named_semaphore sem(open_or_create_t(), name.c_str(), initial_count);
}


[[cpp11::register]]
bool cpp_sem_remove(std::string name) {
  return named_semaphore::remove(name.c_str());
}




[[cpp11::register]]
bool cpp_sem_post(std::string name) {
  named_semaphore sem(open_only_t(), name.c_str());
  sem.post();
  return true;
}


[[cpp11::register]]
bool cpp_sem_wait(std::string name) {
  named_semaphore sem(open_only_t(), name.c_str());
  sem.wait();
  return true;
}


[[cpp11::register]]
bool cpp_sem_try_wait(std::string name) {
  named_semaphore sem(open_only_t(), name.c_str());
  return sem.try_wait();
}


[[cpp11::register]]
bool cpp_sem_timed_wait(std::string name, long timeout_ms) {
  named_semaphore sem(open_only_t(), name.c_str());
  pt::ptime timeout = pt::microsec_clock::universal_time() + pt::milliseconds(timeout_ms);
  return sem.timed_wait(timeout);
}
